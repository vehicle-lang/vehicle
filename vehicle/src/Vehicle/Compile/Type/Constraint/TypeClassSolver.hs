module Vehicle.Compile.Type.Constraint.TypeClassSolver
  ( solveTypeClassConstraint,
  )
where

import Control.Monad (MonadPlus (..), forM)
import Control.Monad.Except (throwError)
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Constraint.LinearitySolver
import Vehicle.Compile.Type.Constraint.PolaritySolver
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.Normalised
import Vehicle.Libraries.StandardLibrary (pattern TensorIdent)
import Vehicle.Libraries.StandardLibrary.Names

--------------------------------------------------------------------------------
-- Solver

solveTypeClassConstraint :: TCM m => WithContext TypeClassConstraint -> m ()
solveTypeClassConstraint constraint@(WithContext (Has m tc spine) ctx) = do
  progress <- solve tc constraint (argExpr <$> spine)
  case progress of
    Left metas -> do
      let blockedConstraint = blockConstraintOn (mapObject TypeClassConstraint constraint) metas
      addConstraints [blockedConstraint]
    Right (newConstraints, solution) -> do
      solution1 <- quote (contextDBLevel ctx) solution
      solveMeta m solution1 (boundContext ctx)
      addConstraints newConstraints

solve :: TypeClass -> TypeClassSolver
solve = \case
  HasEq eq -> solveHasEq eq
  HasOrd ord -> solveHasOrd ord
  HasAnd -> solveHasAnd
  HasOr -> solveHasOr
  HasImplies -> solveHasImplies
  HasIf -> solveHasIf
  HasQuantifier q -> solveHasQuantifier q
  HasNeg -> solveHasNeg
  HasMul -> solveHasMul
  HasDiv -> solveHasDiv
  HasQuantifierIn q -> solveHasQuantifierIn q
  HasNatLits n -> solveHasNatLits n
  HasVecLits n -> solveHasVecLits n
  AlmostEqualConstraint -> solveAlmostEqual
  NatInDomainConstraint n -> solveInDomain n
  LinearityTypeClass tc -> castAuxiliaryFn $ solveLinearityConstraint tc
  PolarityTypeClass tc -> castAuxiliaryFn $ solvePolarityConstraint tc
  tc -> \_ _ ->
    compilerDeveloperError $
      "Expected the class" <+> quotePretty tc <+> "to be solved via instance search"

-- A temporary hack until we separate out the solvers properly.
castAuxiliaryFn :: AuxiliaryTypeClassSolver -> TypeClassSolver
castAuxiliaryFn f c e = castProgress (provenanceOf (contextOf c)) <$> f c e

castProgress :: Provenance -> ConstraintProgress -> TypeClassProgress
castProgress c = \case
  Stuck metas -> Left metas
  Progress newConstraints -> irrelevant c newConstraints

--------------------------------------------------------------------------------
-- HasEq

solveHasEq :: EqualityOp -> TypeClassSolver
solveHasEq op c [arg1, arg2, res]
  | allOf args isMeta = blockOnMetas args
  | anyOf args isIndexType = solveIndexComparisonOp ctx arg1 arg2 res (Equals EqIndex op)
  | anyOf args isNatType = solveSimpleComparisonOp ctx arg1 arg2 res (Equals EqNat op)
  | anyOf args isIntType = solveSimpleComparisonOp ctx arg1 arg2 res (Equals EqInt op)
  | anyOf args isRatType = solveRatComparisonOp ctx arg1 arg2 res (Equals EqRat op)
  | anyOf args isBoolType = solveBoolEquals ctx arg1 arg2 res op
  | anyOf args isVectorType = solveVectorEquals ctx arg1 arg2 res op
  | otherwise = blockOrThrowErrors ctx args tcError
  where
    ctx = contextOf c
    args = [arg1, arg2]
    allowedTypes = map pretty [Bool, Index, Nat, Int, Rat, List, Vector] <> [pretty $ identifierName TensorIdent]
    tcError =
      tcArgError ctx arg1 (EqualsTC op) allowedTypes 1 2
        <> tcArgError ctx arg2 (EqualsTC op) allowedTypes 2 2
solveHasEq _ c _ = malformedConstraintError c

type HasEqSolver =
  forall m.
  TCM m =>
  ConstraintContext ->
  NormType ->
  NormType ->
  NormType ->
  EqualityOp ->
  m TypeClassProgress

solveBoolEquals :: HasEqSolver
solveBoolEquals c arg1 arg2 res op = do
  let p = provenanceOf c
  constraints <- checkBoolTypesEqualUpTo c res [arg1, arg2] MaxLinearity (EqPolarity op)
  let solution = VFreeVar p (identifierOf StdEqualsBool) []
  return $ Right (constraints, solution)

solveVectorEquals :: HasEqSolver
solveVectorEquals c arg1 arg2 res op = do
  dim <- freshDimMeta c
  (arg1Eq, tElem1) <- unifyWithVectorType c dim arg1
  (arg2Eq, tElem2) <- unifyWithVectorType c dim arg2

  -- Recursively check that the element types have equality.
  (metaExpr, recEq) <- createTC c (HasEq op) [tElem1, tElem2, res]

  let p = provenanceOf c
  let solution =
        VFreeVar
          p
          (identifierOf StdEqualsVector)
          [ ImplicitArg p tElem1,
            ImplicitArg p (normalised dim),
            InstanceArg p metaExpr
          ]

  return $ Right ([arg1Eq, arg2Eq, recEq], solution)

--------------------------------------------------------------------------------
-- HasOrd

solveHasOrd :: OrderOp -> TypeClassSolver
solveHasOrd op c [arg1, arg2, res]
  | allOf args isMeta = blockOnMetas args
  | anyOf args isIndexType = solveIndexComparisonOp ctx arg1 arg2 res (Order OrderIndex op)
  | anyOf args isNatType = solveSimpleComparisonOp ctx arg1 arg2 res (Order OrderNat op)
  | anyOf args isIntType = solveSimpleComparisonOp ctx arg1 arg2 res (Order OrderInt op)
  | anyOf args isRatType = solveRatComparisonOp ctx arg1 arg2 res (Order OrderRat op)
  | otherwise = blockOrThrowErrors ctx args tcError
  where
    ctx = contextOf c
    args = [arg1, arg2]
    allowedTypes = fmap pretty [Index, Nat, Int, Rat]
    tcError =
      tcArgError ctx arg1 (OrderTC op) allowedTypes 1 2
        <> tcArgError ctx arg2 (OrderTC op) allowedTypes 1 2
solveHasOrd _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasAndOr

solveHasBoolOp2 :: PolarityTypeClass -> Builtin -> TypeClassSolver
solveHasBoolOp2 polConstraint solutionBuiltin c [arg1, arg2, res] = do
  let ctx = contextOf c
  let p = provenanceOf ctx
  constraints <- checkBoolTypesEqualUpTo ctx res [arg1, arg2] MaxLinearity polConstraint
  let solution = VBuiltin p solutionBuiltin []
  return $ Right (constraints, solution)
solveHasBoolOp2 _ _ c _ = malformedConstraintError c

solveHasAnd :: TypeClassSolver
solveHasAnd = solveHasBoolOp2 MaxPolarity And

solveHasOr :: TypeClassSolver
solveHasOr = solveHasBoolOp2 MaxPolarity Or

solveHasImplies :: TypeClassSolver
solveHasImplies = solveHasBoolOp2 ImpliesPolarity Implies

--------------------------------------------------------------------------------
-- HasQuantifier

solveHasQuantifier :: Quantifier -> TypeClassSolver
solveHasQuantifier _ _ [lamType, _]
  | isMeta lamType = blockOnMetas [lamType]
solveHasQuantifier q c [VPi _ binder body, res]
  | isMeta domain = blockOnMetas [domain]
  | isIndexType domain = solveIndexQuantifier q ctx binder body res
  | isNatType domain = solveNatQuantifier q ctx binder body res
  | isIntType domain = solveIntQuantifier q ctx binder body res
  | isRatType domain = solveRatQuantifier q ctx binder body res
  | isVectorType domain = solveVectorQuantifier q ctx binder body res
  | otherwise = blockOrThrowErrors ctx [domain] tcError
  where
    ctx = contextOf c
    domain = typeOf binder
    tcError = [FailedQuantifierConstraintDomain ctx domain q]
solveHasQuantifier _ c _ = malformedConstraintError c

type HasQuantifierSolver =
  forall m.
  TCM m =>
  Quantifier ->
  ConstraintContext ->
  NormBinder ->
  NormType ->
  NormType ->
  m TypeClassProgress

solveIndexQuantifier :: HasQuantifierSolver
solveIndexQuantifier q c domainBinder body res = do
  let p = provenanceOf c

  (domainEq, indexSize) <- unifyWithIndexType c (typeOf domainBinder)
  (bodyEq, _, _) <- unifyWithAnnBoolType c body
  let resEq = unify c res body

  let method = identifierOf $ if q == Forall then StdForallIndex else StdExistsIndex
  let solution =
        VFreeVar
          p
          method
          [ ImplicitArg p (normalised indexSize)
          ]

  return $ Right ([domainEq, bodyEq, resEq], solution)

solveNatQuantifier :: HasQuantifierSolver
solveNatQuantifier q c _domainBinder body res = do
  let p = provenanceOf c
  (bodyEq, _, _) <- unifyWithAnnBoolType c body
  let resEq = unify c res body

  let solution = case q of
        Forall -> PostulateForallNat
        Exists -> PostulateExistsNat

  return $ Right ([bodyEq, resEq], VFreeVar p solution [])

solveIntQuantifier :: HasQuantifierSolver
solveIntQuantifier q c _domainBinder body res = do
  let p = provenanceOf c
  (bodyEq, _, _) <- unifyWithAnnBoolType c body
  let resEq = unify c res body

  let solution = case q of
        Forall -> PostulateForallInt
        Exists -> PostulateForallInt

  return $ Right ([bodyEq, resEq], VFreeVar p solution [])

solveRatQuantifier :: HasQuantifierSolver
solveRatQuantifier q c domainBinder body res = do
  let p = provenanceOf c

  -- The rational being quantified over is, by definition, linear
  let varName = getBinderName domainBinder
  let domainLin = VLinearityExpr p (Linear (QuantifiedVariableProvenance (provenanceOf domainBinder) varName))
  let domainEq = unify c (typeOf domainBinder) (mkVAnnRatType p domainLin)

  -- The body must be of some Bool type
  (bodyEq, bodyLin, bodyPol) <- unifyWithAnnBoolType c body

  -- Generate a new polarity for the result type and relate it to the polarity of the body.
  resPol <- freshPolarityMeta p
  (_, polTC) <- createTC c (PolarityTypeClass (AddPolarity q)) [normalised bodyPol, normalised resPol]

  -- The result type is the Bool type with the same linearity as the body.
  let resEq = unify c res (mkVAnnBoolType p (normalised bodyLin) (normalised resPol))

  let solution = case q of
        Forall -> PostulateForallRat
        Exists -> PostulateExistsRat

  return $ Right ([domainEq, polTC, bodyEq, resEq], VFreeVar p solution [])

solveVectorQuantifier :: HasQuantifierSolver
solveVectorQuantifier q c domainBinder body res = do
  let p = provenanceOf c
  dim <- freshDimMeta c
  (domainEq, vecElem) <- unifyWithVectorType c dim (typeOf domainBinder)

  -- Recursively check that you can quantify over it.
  let elemDomainBinder = replaceBinderType vecElem domainBinder
  (metaExpr, recTC) <- createTC c (HasQuantifier q) [VPi p elemDomainBinder body, res]

  let method = if q == Forall then StdForallVector else StdExistsVector
  let solution =
        VFreeVar
          p
          (identifierOf method)
          [ ImplicitArg p vecElem,
            ImplicitArg p (normalised dim),
            InstanceArg p metaExpr
          ]

  return $ Right ([domainEq, recTC], solution)

--------------------------------------------------------------------------------
-- HasNeg

solveHasNeg :: TypeClassSolver
solveHasNeg c [arg, res]
  | allOf types isMeta = blockOnMetas [arg, res]
  | anyOf types isIntType = solveNeg ctx res arg NegInt
  | anyOf types isRatType = solveNeg ctx res arg NegRat
  | otherwise = blockOrThrowErrors ctx types tcError
  where
    ctx = contextOf c
    types = [arg, res]
    allowedTypes = fmap pretty [Int, Rat]
    tcError =
      tcArgError ctx arg NegTC allowedTypes 1 1
        <> tcResultError ctx res NegTC allowedTypes
solveHasNeg c _ = malformedConstraintError c

solveNeg ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  NormType ->
  NegDomain ->
  m TypeClassProgress
solveNeg c arg res dom = do
  let p = provenanceOf c
  let eq = unify c res arg
  let solution = VBuiltin p (Neg dom) []
  return $ Right ([eq], solution)

--------------------------------------------------------------------------------
-- HasMul

solveHasMul :: TypeClassSolver
solveHasMul c types@[arg1, arg2, res]
  | allOf types isMeta = blockOnMetas types
  | anyOf types isNatType = solveMulNat ctx arg1 arg2 res
  | anyOf types isIntType = solveMulInt ctx arg1 arg2 res
  | anyOf types isRatType = solveMulRat ctx arg1 arg2 res
  | otherwise = blockOrThrowErrors ctx types tcError
  where
    ctx = contextOf c
    allowedTypes = fmap pretty [Nat, Int, Rat]
    tcError =
      tcArgError ctx arg1 MulTC allowedTypes 1 2
        <> tcArgError ctx arg2 MulTC allowedTypes 2 2
        <> tcResultError ctx res MulTC allowedTypes
solveHasMul c _ = malformedConstraintError c

type HasMulSolver =
  forall m.
  TCM m =>
  ConstraintContext ->
  NormType ->
  NormType ->
  NormType ->
  m TypeClassProgress

solveMulNat :: HasMulSolver
solveMulNat c arg1 arg2 res = do
  let p = provenanceOf c
  constraints <- checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = VBuiltin p (Mul MulNat) []
  return $ Right (constraints, solution)

solveMulInt :: HasMulSolver
solveMulInt c arg1 arg2 res = do
  let p = provenanceOf c
  constraints <- checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = VBuiltin p (Mul MulInt) []
  return $ Right (constraints, solution)

solveMulRat :: HasMulSolver
solveMulRat c arg1 arg2 res = do
  let p = provenanceOf c
  constraints <- checkRatTypesEqualUpTo c res [arg1, arg2] MulLinearity
  let solution = VBuiltin p (Mul MulRat) []
  return $ Right (constraints, solution)

--------------------------------------------------------------------------------
-- HasDiv

solveHasDiv :: TypeClassSolver
solveHasDiv c types@[arg1, arg2, res]
  | allOf types isMeta = blockOnMetas types
  | anyOf types isRatType = solveRatDiv ctx arg1 arg2 res
  | otherwise = blockOrThrowErrors ctx types tcError
  where
    ctx = contextOf c
    allowedTypes = fmap pretty [Rat]
    tcError =
      tcArgError ctx arg1 DivTC allowedTypes 1 2
        <> tcArgError ctx arg2 DivTC allowedTypes 2 2
        <> tcResultError ctx res DivTC allowedTypes
solveHasDiv c _ = malformedConstraintError c

solveRatDiv ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  NormType ->
  NormType ->
  m TypeClassProgress
solveRatDiv c arg1 arg2 res = do
  constraints <- checkRatTypesEqualUpTo c res [arg1, arg2] MulLinearity
  let solution = VBuiltin (provenanceOf c) (Div DivRat) []
  return $ Right (constraints, solution)

--------------------------------------------------------------------------------
-- HasQuantifierIn

solveHasQuantifierIn :: Quantifier -> TypeClassSolver
solveHasQuantifierIn q c [tElem, tCont, tRes] = case tCont of
  (getMeta -> Just {}) -> blockOnMetas [tCont]
  VListType _ tListElem -> do
    let p = provenanceOf ctx
    let elemEq = unify ctx tElem tListElem
    (resEq, _, _) <- unifyWithAnnBoolType ctx tRes
    let method = if q == Forall then StdForallInList else StdExistsInList
    let solution = VFreeVar p (identifierOf method) [ImplicitArg p tElem, ImplicitArg p tRes]
    return $ Right ([elemEq, resEq], solution)
  VVectorType _ tVecElem dim -> do
    let p = provenanceOf ctx
    let elemEq = unify ctx tElem tVecElem
    (resEq, _, _) <- unifyWithAnnBoolType ctx tRes
    let method = identifierOf $ if q == Forall then StdForallInVector else StdExistsInVector
    let solution = VFreeVar p method [ImplicitArg p tElem, ImplicitArg p dim, ImplicitArg p tRes]
    return $ Right ([elemEq, resEq], solution)
  _ -> blockOrThrowErrors ctx [tCont] [tcError]
  where
    ctx = contextOf c
    tcError = FailedQuantInConstraintContainer ctx tCont q
solveHasQuantifierIn _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasIf

solveHasIf :: TypeClassSolver
solveHasIf c [tCond, tArg1, tArg2, tRes]
  | allOf [tArg1, tArg2, tRes] isMeta = blockOnMetas [tArg1, tArg2, tRes]
  | otherwise = do
      (tCondEq, condLin, condPol) <- unifyWithAnnBoolType ctx tCond
      argEqs <- checkSubtypes ctx tRes [tArg1, tArg2]
      (_, linTC) <- createTC ctx (LinearityTypeClass IfCondLinearity) [normalised condLin]
      (_, polTC) <- createTC ctx (PolarityTypeClass IfCondPolarity) [normalised condPol]
      return $ irrelevant ctx $ tCondEq : linTC : polTC : argEqs
  where
    ctx = contextOf c
solveHasIf c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasNatLits

solveHasNatLits :: Int -> TypeClassSolver
solveHasNatLits n c [arg]
  | isMeta arg = blockOnMetas [arg]
  | isIndexType arg = solveFromNatToIndex ctx n arg
  | isNatType arg = solveSimpleFromNat FromNatToNat ctx n arg
  | isIntType arg = solveSimpleFromNat FromNatToInt ctx n arg
  | isRatType arg = solveFromNatToRat ctx n arg
  | otherwise = blockOrThrowErrors ctx [arg] [tcError]
  where
    ctx = contextOf c
    tcError = FailedNatLitConstraint ctx n arg
solveHasNatLits _ c _ = malformedConstraintError c

type HasFromNatSolver =
  forall m.
  TCM m =>
  ConstraintContext ->
  Int ->
  NormType ->
  m TypeClassProgress

solveSimpleFromNat :: FromNatDomain -> HasFromNatSolver
solveSimpleFromNat dom c n _arg = do
  let p = provenanceOf c
  let solution = VBuiltin p (FromNat n dom) []
  return $ Right ([], solution)

solveFromNatToIndex :: HasFromNatSolver
solveFromNatToIndex c n arg = do
  let p = provenanceOf c
  (indexEq, index) <- unifyWithIndexType c arg
  let solution = VBuiltin p (FromNat n FromNatToIndex) [ImplicitArg p (normalised index)]
  return $ Right ([indexEq], solution)

solveFromNatToRat :: HasFromNatSolver
solveFromNatToRat c n arg = do
  let p = provenanceOf c
  let lin = VLinearityExpr (provenanceOf c) Constant
  let ratEq = unify c arg (mkVAnnRatType p lin)
  let solution = VBuiltin p (FromNat n FromNatToRat) []
  return $ Right ([ratEq], solution)

--------------------------------------------------------------------------------
-- HasConLits

solveHasVecLits :: Int -> TypeClassSolver
solveHasVecLits n c [tElem, tCont] = case tCont of
  (getMeta -> Just {}) -> blockOnMetas [tCont]
  VListType _ tListElem -> do
    let p = provenanceOf ctx
    let elemEq = unify ctx tElem tListElem
    let solution = VBuiltin p (FromVec n FromVecToList) [ImplicitArg p tListElem]
    return $ Right ([elemEq], solution)
  VVectorType _ tVecElem dim -> do
    let p = provenanceOf ctx
    let elemEq = unify ctx tElem tVecElem
    let dimEq = unify ctx dim (VNatLiteral p n)
    let solution = VBuiltin p (FromVec n FromVecToVec) [ImplicitArg p tVecElem]
    return $ Right ([elemEq, dimEq], solution)
  _ -> blockOrThrowErrors ctx [tCont] [tcError]
  where
    ctx = contextOf c
    tcError = FailedConLitConstraint ctx tCont
solveHasVecLits _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- AlmostEqual

solveAlmostEqual :: TypeClassSolver
solveAlmostEqual c [targetType, subTypesExpr]
  | allOf types isMeta = blockOnMetas types
  | otherwise = irrelevant ctx <$> checkSubtypes ctx targetType subTypes
  where
    ctx = contextOf c
    subTypes = getConcreteList subTypesExpr
    types = targetType : subTypes
solveAlmostEqual c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- LessThan

solveInDomain :: Int -> TypeClassSolver
solveInDomain n c [arg] = case arg of
  (getMeta -> Just {}) -> blockOnMetas [arg]
  VIndexType _ size -> case size of
    (getMeta -> Just {}) -> blockOnMetas [size]
    (VBuiltin _ (TypeClassOp FromNatTC {}) (_ : InstanceArg _ inst@VMeta {} : _)) ->
      blockOnMetas [inst]
    VNatLiteral _ m
      | m > n -> return $ irrelevant ctx []
      | otherwise -> throwError $ FailedNatLitConstraintTooBig ctx n m
    _ -> throwError $ FailedNatLitConstraintUnknown ctx n size
  VNatType {} -> return $ Right ([], VUnitLiteral p)
  VIntType {} -> return $ Right ([], VUnitLiteral p)
  VAnnRatType {} -> return $ Right ([], VUnitLiteral p)
  _ -> malformedConstraintError c
  where
    ctx = contextOf c
    p = provenanceOf ctx
solveInDomain _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- Subtyping

type SubtypingCheck m =
  TCM m =>
  ConstraintContext ->
  NormType ->
  [NormType] ->
  m [WithContext Constraint]

checkSubtypes :: SubtypingCheck m
checkSubtypes c targetType subTypes
  | anyOf types isRatType = checkRatSubtypes c targetType subTypes
  | anyOf types isBoolType = checkBoolSubtypes c targetType subTypes
  | anyOf types isListType = checkListSubtypes c targetType subTypes
  | anyOf types isVectorType = checkVectorSubtypes c targetType subTypes
  | otherwise = checkSimpleSubtypes c targetType subTypes
  where
    types = targetType : subTypes

checkBoolSubtypes :: SubtypingCheck m
checkBoolSubtypes c targetType subTypes = do
  checkBoolTypesEqualUpTo c targetType subTypes MaxLinearity MaxPolarity

checkRatSubtypes :: SubtypingCheck m
checkRatSubtypes c targetType subTypes = do
  checkRatTypesEqualUpTo c targetType subTypes MaxLinearity

checkListSubtypes :: SubtypingCheck m
checkListSubtypes c targetType subTypes = do
  let p = provenanceOf c
  (targetEqConstraint, targetElemType) <- unifyWithListType c targetType
  (subEqConstraints, subElemTypes) <- unzip <$> forM subTypes (unifyWithListType c)
  let subElemTypeSeq = mkVList p (VTypeUniverse p 0) (fmap normalised subElemTypes)
  (_, recEq) <- createTC c AlmostEqualConstraint [normalised targetElemType, subElemTypeSeq]
  return (targetEqConstraint : recEq : subEqConstraints)

checkVectorSubtypes :: SubtypingCheck m
checkVectorSubtypes c targetType subTypes = do
  let p = provenanceOf c
  dim <- freshDimMeta c
  (targetEqConstraint, targetElemType) <- unifyWithVectorType c dim targetType
  (subEqConstraints, subElemTypes) <- unzip <$> forM subTypes (unifyWithVectorType c dim)
  let subElemTypeSeq = mkVList p (VTypeUniverse p 0) subElemTypes
  (_, recEq) <- createTC c AlmostEqualConstraint [targetElemType, subElemTypeSeq]
  return (targetEqConstraint : recEq : subEqConstraints)

checkSimpleSubtypes :: SubtypingCheck m
checkSimpleSubtypes c targetType subTypes = do
  let types = targetType : subTypes
  let adjacentPairs = zip types $ tail types
  let constraints = map (uncurry (unify c)) adjacentPairs
  return constraints

--------------------------------------------------------------------------------
-- Utilities

checkBoolTypesEqualUpTo ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  [NormType] ->
  LinearityTypeClass ->
  PolarityTypeClass ->
  m [WithContext Constraint]
checkBoolTypesEqualUpTo c targetType subTypes linTC polTC = do
  (targetEqConstraint, targetLin, targetPol) <-
    unifyWithAnnBoolType c targetType

  (subEqConstraints, subLins, subPols) <-
    unzip3 <$> forM subTypes (unifyWithAnnBoolType c)

  let polCombine = combineAuxiliaryConstraints (PolarityTypeClass polTC) (Polarity Unquantified)
  let linCombine = combineAuxiliaryConstraints (LinearityTypeClass linTC) (Linearity Constant)
  linTCConstraints <- linCombine freshLinearityMeta c (normalised targetLin) subLins
  polTCConstraints <- polCombine freshPolarityMeta c (normalised targetPol) subPols

  return $ targetEqConstraint : subEqConstraints <> linTCConstraints <> polTCConstraints

checkRatTypesEqualUpTo ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  [NormType] ->
  LinearityTypeClass ->
  m [WithContext Constraint]
checkRatTypesEqualUpTo c targetType subTypes linTC = do
  (targetEqConstraint, targetLin) <-
    unifyWithAnnRatType c targetType

  (subEqConstraints, subLins) <-
    unzip <$> forM subTypes (unifyWithAnnRatType c)

  let linCombine = combineAuxiliaryConstraints (LinearityTypeClass linTC) (Linearity Constant)
  linTCConstraints <- linCombine freshLinearityMeta c (normalised targetLin) subLins

  return $ targetEqConstraint : subEqConstraints <> linTCConstraints

checkOp2SimpleTypesEqual ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  NormType ->
  NormType ->
  m [WithContext Constraint]
checkOp2SimpleTypesEqual c arg1 arg2 res = do
  let argsEq = unify c arg1 arg2
  let resEq = unify c arg1 res
  return [argsEq, resEq]

createTC ::
  TCM m =>
  ConstraintContext ->
  TypeClass ->
  [NormType] ->
  m (NormExpr, WithContext Constraint)
createTC c tc argExprs = do
  let p = provenanceOf c
  let ctx = copyContext c
  let dbLevel = contextDBLevel c
  let nArgs = ExplicitArg p <$> argExprs
  newTypeClassExpr <- quote dbLevel (VConstructor p (TypeClass tc) nArgs)
  (meta, metaExpr) <- freshTypeClassPlacementMeta p newTypeClassExpr (boundContext c)
  return (normalised metaExpr, WithContext (TypeClassConstraint (Has meta tc nArgs)) ctx)

unifyWithAnnBoolType ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  m (WithContext Constraint, GluedExpr, GluedExpr)
unifyWithAnnBoolType c t = do
  let p = provenanceOf c
  lin <- freshLinearityMeta p
  pol <- freshPolarityMeta p
  let eq = unify c t (mkVAnnBoolType p (normalised lin) (normalised pol))
  return (eq, lin, pol)

unifyWithIndexType ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  m (WithContext Constraint, GluedExpr)
unifyWithIndexType c t = do
  let p = provenanceOf c
  indexSize <- freshExprMeta p (NatType p) (boundContext c)
  let eq = unify c t (mkVIndexType p (normalised indexSize))
  return (eq, indexSize)

unifyWithAnnRatType ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  m (WithContext Constraint, GluedExpr)
unifyWithAnnRatType c t = do
  let p = provenanceOf c
  lin <- freshLinearityMeta p
  let eq = unify c t (mkVAnnRatType p (normalised lin))
  return (eq, lin)

unifyWithListType ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  m (WithContext Constraint, GluedType)
unifyWithListType c t = do
  let p = provenanceOf c
  elemType <- freshExprMeta p (TypeUniverse p 0) (boundContext c)
  let eq = unify c t (mkVListType p (normalised elemType))
  return (eq, elemType)

unifyWithVectorType ::
  TCM m =>
  ConstraintContext ->
  GluedExpr ->
  NormType ->
  m (WithContext Constraint, NormType)
unifyWithVectorType c dim t = do
  let p = provenanceOf c
  elemType <- freshExprMeta p (TypeUniverse p 0) (boundContext c)
  let eq = unify c t (mkVVecType p (normalised elemType) (normalised dim))
  return (eq, normalised elemType)

freshDimMeta :: TCM m => ConstraintContext -> m GluedExpr
freshDimMeta c = do
  let p = provenanceOf c
  freshExprMeta p (NatType p) (boundContext c)

solveSimpleComparisonOp ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  NormType ->
  NormType ->
  Builtin ->
  m TypeClassProgress
solveSimpleComparisonOp c arg1 arg2 res solution = do
  let p = provenanceOf c
  let resEq = unify c res (mkVAnnBoolType p (VLinearityExpr p Constant) (VPolarityExpr p Unquantified))
  let argEq = unify c arg1 arg2
  return $ Right ([argEq, resEq], VBuiltin p solution [])

solveIndexComparisonOp ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  NormType ->
  NormType ->
  Builtin ->
  m TypeClassProgress
solveIndexComparisonOp c arg1 arg2 res solution = do
  let p = provenanceOf c
  (arg1Eq, _size1) <- unifyWithIndexType c arg1
  (arg2Eq, _size2) <- unifyWithIndexType c arg2
  let resEq = unify c res (mkVAnnBoolType p (VLinearityExpr p Constant) (VPolarityExpr p Unquantified))
  return $ Right ([arg1Eq, arg2Eq, resEq], VBuiltin p solution [])

solveRatComparisonOp ::
  TCM m =>
  ConstraintContext ->
  NormType ->
  NormType ->
  NormType ->
  Builtin ->
  m TypeClassProgress
solveRatComparisonOp c arg1 arg2 res op = do
  let p = provenanceOf c

  (arg1Eq, arg1Lin) <- unifyWithAnnRatType c arg1
  (arg2Eq, arg2Lin) <- unifyWithAnnRatType c arg2
  (resEq, resLin, resPol) <- unifyWithAnnBoolType c res

  -- The resulting linearity is the max of the linearity of the arguments.
  (_meta, linTC) <-
    createTC
      c
      (LinearityTypeClass MaxLinearity)
      [normalised arg1Lin, normalised arg2Lin, normalised resLin]
  -- The polarity is unquantified.
  let polEq = unify c (normalised resPol) (VPolarityExpr p Unquantified)

  return $ Right ([arg1Eq, arg2Eq, resEq, linTC, polEq], VBuiltin p op [])

combineAuxiliaryConstraints ::
  forall m.
  TCM m =>
  TypeClass ->
  BuiltinConstructor ->
  (Provenance -> m GluedExpr) ->
  ConstraintContext ->
  NormExpr ->
  [GluedExpr] ->
  m [WithContext Constraint]
combineAuxiliaryConstraints tc unit makeMeta c result auxs = do
  (res, tcConstraints) <- foldPairs auxs
  let resEq = unify c (normalised res) result
  return $ resEq : tcConstraints
  where
    foldPairs :: [GluedExpr] -> m (GluedExpr, [WithContext Constraint])
    foldPairs [] = do
      let unUnit = Builtin mempty (Constructor unit)
      let nUnit = VBuiltin mempty (Constructor unit) []
      return (Glued unUnit nUnit, [])
    foldPairs [a] = return (a, [])
    foldPairs (a : cs) = do
      (b, constraints) <- foldPairs cs
      res <- makeMeta (provenanceOf c)
      (_, tc1) <- createTC c tc [normalised a, normalised b, normalised res]
      return (res, tc1 : constraints)

irrelevant :: HasProvenance a => a -> [WithContext Constraint] -> TypeClassProgress
irrelevant c newConstraints = Right (newConstraints, VUnitLiteral (provenanceOf c))

blockOnMetas :: TCM m => [NormExpr] -> m TypeClassProgress
blockOnMetas args = do
  let metas = mapMaybe getMeta args
  progress <- blockOn metas
  return $ castProgress mempty progress

blockOrThrowErrors ::
  TCM m =>
  ConstraintContext ->
  [NormExpr] ->
  [CompileError] ->
  m TypeClassProgress
blockOrThrowErrors _ args err = do
  -- TODO forcing should be incorporated beforehand.
  blockingMetas <- MetaSet.unions . fmap snd <$> traverse forceHead args
  if MetaSet.null blockingMetas
    then throwError (head err)
    else return $ Left blockingMetas

anyOf :: [a] -> (a -> Bool) -> Bool
anyOf = flip any

allOf :: [a] -> (a -> Bool) -> Bool
allOf = flip all

unless2 :: MonadPlus m => Bool -> a -> m a
unless2 p a = if not p then return a else mzero

getConcreteList :: NormExpr -> [NormExpr]
getConcreteList = \case
  VBuiltin _ (Constructor Nil) _ -> []
  VBuiltin _ (Constructor Cons) [_, x, xs] -> argExpr x : getConcreteList (argExpr xs)
  _ -> developerError "Malformed concrete list"

tcArgError ::
  ConstraintContext ->
  NormType ->
  TypeClassOp ->
  [UnAnnDoc] ->
  Int ->
  Int ->
  [CompileError]
tcArgError c arg op allowedTypes argIndex numberOfArgs =
  unless2
    (isMeta arg)
    (FailedBuiltinConstraintArgument c (TypeClassOp op) arg allowedTypes argIndex numberOfArgs)

tcResultError ::
  ConstraintContext ->
  NormType ->
  TypeClassOp ->
  [UnAnnDoc] ->
  [CompileError]
tcResultError c result op allowedTypes =
  unless2
    (isMeta result)
    (FailedBuiltinConstraintResult c (TypeClassOp op) result allowedTypes)
