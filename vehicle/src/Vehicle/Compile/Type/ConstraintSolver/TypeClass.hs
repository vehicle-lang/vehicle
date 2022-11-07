module Vehicle.Compile.Type.ConstraintSolver.TypeClass
  ( solveTypeClassConstraint
  ) where

import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe)

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.ConstraintSolver.Core
import Vehicle.Compile.Type.ConstraintSolver.Linearity
import Vehicle.Compile.Type.ConstraintSolver.Polarity
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Monad
import Vehicle.Language.StandardLibrary.Names

--------------------------------------------------------------------------------
-- Public interface

solveTypeClassConstraint :: TCM m
                         => ConstraintContext
                         -> TypeClassConstraint
                         -> m ConstraintProgress
solveTypeClassConstraint ctx c@(Has m tc args) = do
  nfArgs <- traverse whnf (onlyExplicit args)
  progress <- solve tc (TC ctx c) nfArgs

  case progress of
    Left  metas                      -> return $ Stuck metas
    Right (newConstraints, solution) -> do
      metaSolved m solution
      return $ Progress newConstraints

--------------------------------------------------------------------------------
-- Solver

type TypeClassProgress = Either MetaSet ([Constraint], CheckedExpr)

type TypeClassSolver =
  forall m . TCM m
  => Constraint
  -> [CheckedType]
  -> m TypeClassProgress

solve :: TypeClass -> TypeClassSolver
solve = \case
  HasEq eq                -> solveHasEq eq
  HasOrd ord              -> solveHasOrd ord
  HasNot                  -> solveHasNot
  HasAnd                  -> solveHasAnd
  HasOr                   -> solveHasOr
  HasImplies              -> solveHasImplies
  HasQuantifier q         -> solveHasQuantifier q
  HasNeg                  -> solveHasNeg
  HasAdd                  -> solveHasAdd
  HasSub                  -> solveHasSub
  HasMul                  -> solveHasMul
  HasDiv                  -> solveHasDiv
  HasFold                 -> solveHasFold
  HasQuantifierIn q       -> solveHasQuantifierIn q
  HasIf                   -> solveHasIf

  HasNatLits n            -> solveHasNatLits n
  HasRatLits              -> solveHasRatLits
  HasVecLits n            -> solveHasVecLits n

  AlmostEqualConstraint   -> solveAlmostEqual
  NatInDomainConstraint n -> solveInDomain n

  LinearityTypeClass tc   -> castProgressFn $ solveLinearityConstraint tc
  PolarityTypeClass  tc   -> castProgressFn $ solvePolarityConstraint tc

-- A temporary hack until we separate out the solvers properly.
castProgressFn :: TCM m
               => (Constraint -> [CheckedType] -> m ConstraintProgress)
               -> (Constraint -> [CheckedType] -> m TypeClassProgress)
castProgressFn f c e = castProgress (provenanceOf c) <$> f c e

castProgress :: Provenance -> ConstraintProgress -> TypeClassProgress
castProgress c = \case
  Stuck metas             -> Left metas
  Progress newConstraints -> irrelevant c newConstraints

--------------------------------------------------------------------------------
-- HasEq

solveHasEq :: EqualityOp -> TypeClassSolver
solveHasEq op c [arg1, arg2, res]
  | allOf args isMeta        = blockOnMetas args
  | anyOf args isIndexType   = solveIndexComparisonOp  c arg1 arg2 res (Equals EqIndex op)
  | anyOf args isNatType     = solveSimpleComparisonOp c arg1 arg2 res (Equals EqNat op)
  | anyOf args isIntType     = solveSimpleComparisonOp c arg1 arg2 res (Equals EqInt op)
  | anyOf args isAnnRatType  = solveRatComparisonOp    c arg1 arg2 res (Equals EqRat op)
  | anyOf args isAnnBoolType = solveBoolEquals         c arg1 arg2 res op
  | anyOf args isVectorType  = solveVectorEquals       c arg1 arg2 res op
  | otherwise                = blockOrThrowErrors      c args tcError
  where
    args = [arg1, arg2]
    allowedTypes = map Constructor [Bool, Index, Nat, Int, Rat, List, Vector] <> [Tensor]
    tcError =
      tcArgError c arg1 (EqualsTC op) allowedTypes 1 2 <>
      tcArgError c arg2 (EqualsTC op) allowedTypes 2 2


solveHasEq _ c _ = malformedConstraintError c

type HasEqSolver =
  forall m . TCM m
  => Constraint
  -> CheckedType
  -> CheckedType
  -> CheckedType
  -> EqualityOp
  -> m TypeClassProgress

solveBoolEquals :: HasEqSolver
solveBoolEquals c arg1 arg2 res op = do
  let p = provenanceOf c
  constraints <- checkBoolTypesEqualUpTo c res [arg1, arg2] MaxLinearity (EqPolarity op)
  let solution = FreeVar p (identifierOf StdEqualsBool)
  return $ Right (constraints, solution)

{-
solveListEquals :: TCM m
                => EqualityOp
                -> Constraint
                -> CheckedType
                -> CheckedType
                -> CheckedType
                -> m TypeClassProgress
solveListEquals op c arg1 arg2 res = do
  let p = provenanceOf c
  (arg1Eq, tElem1) <- unifyWithListType c arg1
  (arg2Eq, tElem2) <- unifyWithListType c arg2

  -- Recursively check that the element types have equality.
  (meta, recEq) <- createTC c (HasEq op) [tElem1, tElem2, res]
  let solution = FreeVar p StdEqualsBool

  return $ Right ([arg1Eq, arg2Eq, recEq], solution)
-}

solveVectorEquals :: HasEqSolver
solveVectorEquals c arg1 arg2 res op = do
  dim <- freshDimMeta c
  (arg1Eq, tElem1) <- unifyWithVectorType c dim arg1
  (arg2Eq, tElem2) <- unifyWithVectorType c dim arg2

  -- Recursively check that the element types have equality.
  (meta, recEq) <- createTC c (HasEq op) [tElem1, tElem2, res]

  let p = provenanceOf c
  let solution = App p (FreeVar p (identifierOf StdEqualsVector))
        [ ImplicitArg p tElem1
        , ImplicitArg p dim
        , InstanceArg p (Meta p meta)
        ]

  return $ Right ([arg1Eq, arg2Eq, recEq], solution)

--------------------------------------------------------------------------------
-- HasOrd

solveHasOrd :: OrderOp -> TypeClassSolver
solveHasOrd op c [arg1, arg2, res]
  | allOf args isMeta           = do logDebug MaxDetail "hi2"; blockOnMetas args
  | anyOf args isIndexType      = solveIndexComparisonOp  c arg1 arg2 res (Order OrderIndex op)
  | anyOf args isNatType        = solveSimpleComparisonOp c arg1 arg2 res (Order OrderNat   op)
  | anyOf args isIntType        = solveSimpleComparisonOp c arg1 arg2 res (Order OrderInt   op)
  | anyOf args isAnnRatType     = solveRatComparisonOp    c arg1 arg2 res (Order OrderRat   op)
  | otherwise                   = do logDebug MaxDetail "hi"; blockOrThrowErrors c args tcError
  where
    args         = [arg1, arg2]
    allowedTypes = fmap Constructor [Index, Nat, Int, Rat]
    tcError      =
      tcArgError c arg1 (OrderTC op) allowedTypes 1 2 <>
      tcArgError c arg2 (OrderTC op) allowedTypes 1 2
solveHasOrd _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasNot

solveHasNot :: TypeClassSolver
solveHasNot c [arg, res] = do
  let p = provenanceOf c
  (argEq, argLin, argPol) <- unifyWithAnnBoolType c arg
  (resEq, resLin, resPol) <- unifyWithAnnBoolType c res

  let linEq = unify c argLin resLin
  (_, polTC) <- createTC c (PolarityTypeClass NegPolarity) [argPol, resPol]
  let solution = Builtin p Not

  return $ Right ([argEq, resEq, linEq, polTC], solution)

solveHasNot c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasAndOr

solveHasBoolOp2 :: PolarityTypeClass -> Builtin -> TypeClassSolver
solveHasBoolOp2 polConstraint solutionBuiltin c [arg1, arg2, res] = do
  let p = provenanceOf c
  constraints <- checkBoolTypesEqualUpTo c res [arg1, arg2] MaxLinearity polConstraint
  let solution = Builtin p solutionBuiltin
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
solveHasQuantifier q c [Pi _ binder body, res]
  | isMeta        domain = blockOnMetas [domain]
  | isAnnBoolType domain = solveBoolQuantifier   q c binder body res
  | isIndexType   domain = solveIndexQuantifier  q c binder body res
  | isNatType     domain = solveNatQuantifier    q c binder body res
  | isIntType     domain = solveIntQuantifier    q c binder body res
  | isAnnRatType  domain = solveRatQuantifier    q c binder body res
  | isVectorType  domain = solveVectorQuantifier q c binder body res
  | otherwise            = blockOrThrowErrors c [domain] tcError
    where
      domain  = typeOf binder
      tcError = [FailedQuantifierConstraintDomain (constraintContext c) domain q]

solveHasQuantifier _ c _ = malformedConstraintError c

type HasQuantifierSolver
  =  forall m . TCM m
  => Quantifier
  -> Constraint
  -> CheckedBinder
  -> CheckedType
  -> CheckedType
  -> m TypeClassProgress

solveBoolQuantifier :: HasQuantifierSolver
solveBoolQuantifier q c domainBinder body res = do
  let p = provenanceOf c

  -- If we're quantifying over a Bool then it itself is linear and unquantified
  let domainLin = LinearityExpr p Constant
  let domainPol = PolarityExpr p Unquantified
  let domainEq = unify c (typeOf domainBinder) (AnnBoolType p domainLin domainPol)

  -- The body is some unknown annotated boolean
  (bodyEq, _, _) <- unifyWithAnnBoolType c body

  -- The result is equal to the body
  let resEq = unify c res body

  let solution = FreeVar p $ identifierOf $ case q of
        Forall -> StdForallBool
        Exists -> StdExistsBool

  return $ Right ([domainEq, bodyEq, resEq], solution)

solveIndexQuantifier :: HasQuantifierSolver
solveIndexQuantifier q c domainBinder body res = do
  let p = provenanceOf c

  (domainEq, indexSize) <- unifyWithIndexType c (typeOf domainBinder)
  (bodyEq, _, _)        <- unifyWithAnnBoolType c body
  let resEq          = unify c res body

  let method = identifierOf $ if q == Forall then StdForallIndex else StdExistsIndex
  let solution = App p (FreeVar (provenanceOf c) method)
        [ ImplicitArg p indexSize
        ]

  return $ Right ([domainEq, bodyEq, resEq], solution)

solveNatQuantifier :: HasQuantifierSolver
solveNatQuantifier q c _domainBinder body res = do
  (bodyEq, _, _) <- unifyWithAnnBoolType c body
  let resEq          = unify c res body

  let solution = FreeVar (provenanceOf c) $ case q of
        Forall -> PostulateForallNat
        Exists -> PostulateExistsNat

  return $ Right ([bodyEq, resEq], solution)

solveIntQuantifier :: HasQuantifierSolver
solveIntQuantifier q c _domainBinder body res = do
  (bodyEq, _, _) <- unifyWithAnnBoolType c body
  let resEq      = unify c res body

  let solution = FreeVar (provenanceOf c) $ case q of
        Forall -> PostulateForallInt
        Exists -> PostulateForallInt

  return $ Right ([bodyEq, resEq], solution)

solveRatQuantifier :: HasQuantifierSolver
solveRatQuantifier q c domainBinder body res = do
  let p = provenanceOf c

  -- The rational being quantified over is, by definition, linear
  let varName   = getBinderName domainBinder
  let domainLin = LinearityExpr p (Linear (QuantifiedVariableProvenance p varName))
  let domainEq  = unify c (typeOf domainBinder) (AnnRatType p domainLin)

  -- The body must be of some Bool type
  (bodyEq, bodyLin, bodyPol) <- unifyWithAnnBoolType c body

  -- Generate a new polarity for the result type and relate it to the polarity of the body.
  resPol     <- freshPolarityMeta p
  (_, polTC) <- createTC c (PolarityTypeClass (AddPolarity q)) [bodyPol, resPol]

  -- The result type is the Bool type with the same linearity as the body.
  let resEq  = unify c res  (AnnBoolType p bodyLin resPol)

  let solution = FreeVar p $ case q of
        Forall -> PostulateForallRat
        Exists -> PostulateExistsRat

  return $ Right ([domainEq, polTC, bodyEq, resEq], solution)

solveVectorQuantifier :: HasQuantifierSolver
solveVectorQuantifier q c domainBinder body res = do
  let p = provenanceOf c
  dim <- freshDimMeta c
  (domainEq, vecElem) <- unifyWithVectorType c dim (typeOf domainBinder)

  -- Recursively check that you can quantify over it.
  let elemDomainBinder = replaceBinderType vecElem domainBinder
  (meta, recTC) <- createTC c (HasQuantifier q) [Pi p elemDomainBinder body, res]

  let method = if q == Forall then StdForallVector else StdExistsVector
  let solution = App p (FreeVar (provenanceOf c) (identifierOf method))
        [ ImplicitArg p vecElem
        , ImplicitArg p dim
        , InstanceArg p (Meta p meta)
        ]

  return $ Right ([domainEq, recTC], solution)

--------------------------------------------------------------------------------
-- HasNeg

solveHasNeg :: TypeClassSolver
solveHasNeg c [arg, res]
  | allOf types isMeta       = blockOnMetas [arg, res]
  | anyOf types isIntType    = solveNeg c res arg NegInt
  | anyOf types isAnnRatType = solveNeg c res arg NegRat
  | otherwise                = blockOrThrowErrors c types tcError
  where
    types = [arg, res]
    allowedTypes = fmap Constructor [Int, Rat]
    tcError =
      tcArgError    c arg NegTC allowedTypes 1 1 <>
      tcResultError c res NegTC allowedTypes

solveHasNeg c _ = malformedConstraintError c

solveNeg :: TCM m
         => Constraint
         -> CheckedType
         -> CheckedType
         -> NegDomain
         -> m TypeClassProgress
solveNeg c arg res dom = do
  let p = provenanceOf c
  let eq = unify c res arg
  let solution = Builtin p (Neg dom)
  return $ Right ([eq], solution)

--------------------------------------------------------------------------------
-- HasAdd

solveHasAdd :: TypeClassSolver
solveHasAdd c types@[arg1, arg2, res]
  | allOf types isMeta           = blockOnMetas types
  | anyOf types isNatType        = solveAddNat    c arg1 arg2 res
  | anyOf types isIntType        = solveAddInt    c arg1 arg2 res
  | anyOf types isAnnRatType     = solveAddRat    c arg1 arg2 res
  | anyOf types isVectorType     = solveAddVector c arg1 arg2 res
  | otherwise                    = blockOrThrowErrors c types tcError
  where
    allowedTypes = fmap Constructor [Nat, Int, Rat]
    tcError  =
      tcArgError    c arg1 AddTC allowedTypes 1 2 <>
      tcArgError    c arg2 AddTC allowedTypes 2 2 <>
      tcResultError c res  AddTC allowedTypes

solveHasAdd c _ = malformedConstraintError c

type HasAddSolver =
  forall m . TCM m
  => Constraint
  -> CheckedType
  -> CheckedType
  -> CheckedType
  -> m TypeClassProgress

solveAddNat :: HasAddSolver
solveAddNat c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Add AddNat)
  return $ Right (constraints, solution)

solveAddInt :: HasAddSolver
solveAddInt c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Add AddInt)
  return $ Right (constraints, solution)

solveAddRat :: HasAddSolver
solveAddRat c arg1 arg2 res = do
  let p = provenanceOf c
  constraints <- checkRatTypesEqualUpTo c res [arg1, arg2] MaxLinearity
  let solution = Builtin p (Add AddRat)
  return $ Right (constraints, solution)

solveAddVector :: HasAddSolver
solveAddVector c arg1 arg2 res = do
  let p = provenanceOf c
  dim <- freshDimMeta c

  (arg1Eq, arg1Elem) <- unifyWithVectorType c dim arg1
  (arg2Eq, arg2Elem) <- unifyWithVectorType c dim arg2
  (resEq,  resElem)  <- unifyWithVectorType c dim res
  (meta, recTC) <- createTC c HasAdd [arg1Elem, arg2Elem, resElem]

  let constraints = [arg1Eq, arg2Eq, resEq, recTC]
  let solution = App p (FreeVar p (identifierOf StdAddVector))
        [ ImplicitArg p arg1Elem
        , ImplicitArg p arg2Elem
        , ImplicitArg p resElem
        , ImplicitArg p dim
        , InstanceArg p (Meta p meta)
        ]

  return $ Right (constraints, solution)

--------------------------------------------------------------------------------
-- HasSub

solveHasSub :: TypeClassSolver
solveHasSub c types@[arg1, arg2, res]
  | allOf types isMeta           = blockOnMetas types
  | anyOf types isIntType        = solveSubInt    c arg1 arg2 res
  | anyOf types isAnnRatType     = solveSubRat    c arg1 arg2 res
  | anyOf types isVectorType     = solveSubVector c arg1 arg2 res
  | otherwise                    = blockOrThrowErrors c types tcError
  where
    allowedTypes = fmap Constructor [Int, Rat]
    tcError  =
      tcArgError    c arg1 SubTC allowedTypes 1 2 <>
      tcArgError    c arg2 SubTC allowedTypes 2 2 <>
      tcResultError c res  SubTC allowedTypes

solveHasSub c _ = malformedConstraintError c

type HasSubSolver =
  forall m . TCM m
  => Constraint
  -> CheckedType
  -> CheckedType
  -> CheckedType
  -> m TypeClassProgress

solveSubInt :: HasSubSolver
solveSubInt c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Sub SubInt)
  return $ Right (constraints, solution)

solveSubRat :: HasSubSolver
solveSubRat c arg1 arg2 res = do
  let p = provenanceOf c
  constraints <- checkRatTypesEqualUpTo c res [arg1, arg2] MaxLinearity
  let solution = Builtin p (Sub SubRat)
  return $ Right (constraints, solution)

solveSubVector :: HasSubSolver
solveSubVector c arg1 arg2 res = do
  let p = provenanceOf c
  dim <- freshDimMeta c

  (arg1Eq, arg1Elem) <- unifyWithVectorType c dim arg1
  (arg2Eq, arg2Elem) <- unifyWithVectorType c dim arg2
  (resEq,  resElem)  <- unifyWithVectorType c dim res
  (meta, recTC) <- createTC c HasSub [arg1Elem, arg2Elem, resElem]

  let constraints = [arg1Eq, arg2Eq, resEq, recTC]
  let solution = App p (FreeVar p (identifierOf StdSubVector))
        [ ImplicitArg p arg1Elem
        , ImplicitArg p arg2Elem
        , ImplicitArg p resElem
        , ImplicitArg p dim
        , InstanceArg p (Meta p meta)
        ]

  return $ Right (constraints, solution)

--------------------------------------------------------------------------------
-- HasMul

solveHasMul :: TypeClassSolver
solveHasMul c types@[arg1, arg2, res]
  | allOf types isMeta           = blockOnMetas types
  | anyOf types isNatType        = solveMulNat c arg1 arg2 res
  | anyOf types isIntType        = solveMulInt c arg1 arg2 res
  | anyOf types isAnnRatType     = solveMulRat c arg1 arg2 res
  | otherwise                    = blockOrThrowErrors c types tcError
  where
    allowedTypes = fmap Constructor [Nat, Int, Rat]
    tcError  =
      tcArgError    c arg1 MulTC allowedTypes 1 2 <>
      tcArgError    c arg2 MulTC allowedTypes 2 2 <>
      tcResultError c res  MulTC allowedTypes
solveHasMul c _ = malformedConstraintError c

type HasMulSolver =
  forall m . TCM m
  => Constraint
  -> CheckedType
  -> CheckedType
  -> CheckedType
  -> m TypeClassProgress

solveMulNat :: HasMulSolver
solveMulNat c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Mul MulNat)
  return $ Right (constraints, solution)

solveMulInt :: HasMulSolver
solveMulInt c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Mul MulInt)
  return $ Right (constraints, solution)

solveMulRat :: HasMulSolver
solveMulRat c arg1 arg2 res = do
  let p = provenanceOf c
  constraints <- checkRatTypesEqualUpTo c res [arg1, arg2] MulLinearity
  let solution = Builtin p (Mul MulRat)
  return $ Right (constraints, solution)

--------------------------------------------------------------------------------
-- HasDiv

solveHasDiv :: TypeClassSolver
solveHasDiv c types@[arg1, arg2, res]
  | allOf types isMeta           = blockOnMetas types
  | anyOf types isAnnRatType     = solveRatDiv c arg1 arg2 res
  | otherwise                    = blockOrThrowErrors c types tcError
  where
    allowedTypes = fmap Constructor [Rat]
    tcError =
      tcArgError    c arg1 DivTC allowedTypes 1 2 <>
      tcArgError    c arg2 DivTC allowedTypes 2 2 <>
      tcResultError c res  DivTC allowedTypes

solveHasDiv c _ = malformedConstraintError c

solveRatDiv :: TCM m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveRatDiv c arg1 arg2 res = do
  constraints <- checkRatTypesEqualUpTo c res [arg1, arg2] MulLinearity
  let solution = Builtin (provenanceOf c) (Div DivRat)
  return $ Right (constraints, solution)

--------------------------------------------------------------------------------
-- HasFold

solveHasFold :: TypeClassSolver
solveHasFold c [tElem, tCont] = case tCont of
  (exprHead -> Meta{})      -> blockOnMetas [tCont]
  ListType   _ tListElem    -> solveFoldList c tElem tListElem
  VectorType _ tVecElem dim -> solveFoldVec  dim c tElem tVecElem
  _                         -> blockOrThrowErrors c [tCont] [tcError]
    where tcError = FailedFoldConstraintContainer (constraintContext c) tCont

solveHasFold c _ = malformedConstraintError c

type HasFoldSolver =
  forall m . TCM m
  => Constraint
  -> CheckedType
  -> CheckedType
  -> m TypeClassProgress

solveFoldList :: HasFoldSolver
solveFoldList c tElem tListElem = do
  let constraint = unify c tElem tListElem
  let solution = Builtin (provenanceOf c) (Fold FoldList)
  return $ Right ([constraint], solution)

solveFoldVec :: CheckedExpr -> HasFoldSolver
solveFoldVec _dim c tElem tListElem = do
  let constraint = unify c tElem tListElem
  let solution = Builtin (provenanceOf c) (Fold FoldVector)
  return $ Right ([constraint], solution)

--------------------------------------------------------------------------------
-- HasQuantifierIn

solveHasQuantifierIn :: Quantifier -> TypeClassSolver
solveHasQuantifierIn q c [tElem, tCont, tRes] = case tCont of
  (exprHead -> Meta{}) -> blockOnMetas [tCont]

  ListType _ tListElem -> do
    let p = provenanceOf c
    let elemEq = unify c tElem tListElem
    (resEq, _, _) <- unifyWithAnnBoolType c tRes
    let method = if q == Forall then StdForallInList else StdExistsInList
    let solution = App p (FreeVar p (identifierOf method)) [ImplicitArg p tElem, ImplicitArg p tRes]
    return $ Right ([elemEq, resEq], solution)

  VectorType _ tVecElem dim -> do
    let p = provenanceOf c
    let elemEq = unify c tElem tVecElem
    (resEq, _, _) <- unifyWithAnnBoolType c tRes
    let method = identifierOf $ if q == Forall then StdForallInVector else StdExistsInVector
    let solution = App p (FreeVar p method) [ImplicitArg p tElem, ImplicitArg p dim, ImplicitArg p tRes]
    return $ Right ([elemEq, resEq], solution)

  _ -> blockOrThrowErrors c [tCont] [tcError]
    where tcError = FailedQuantInConstraintContainer (constraintContext c) tCont q

solveHasQuantifierIn _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasIf

solveHasIf :: TypeClassSolver
solveHasIf c [tCond, tArg1, tArg2, tRes]
  | allOf [tArg1, tArg2, tRes] isMeta = blockOnMetas [tArg1, tArg2, tRes]
  | otherwise = do
    (tCondEq, condLin, condPol) <- unifyWithAnnBoolType c tCond
    argEqs <- checkSubtypes c tRes [tArg1, tArg2]
    (_, linTC) <- createTC c (LinearityTypeClass IfCondLinearity) [condLin]
    (_, polTC) <- createTC c (PolarityTypeClass IfCondPolarity) [condPol]
    return $ irrelevant c $ tCondEq : linTC : polTC : argEqs

solveHasIf c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasNatLits

solveHasNatLits :: Int -> TypeClassSolver
solveHasNatLits n c [arg]
  | isMeta arg           = blockOnMetas [arg]
  | isIndexType      arg = solveFromNatToIndex c n arg
  | isNatType        arg = solveSimpleFromNat  FromNatToNat c n arg
  | isIntType        arg = solveSimpleFromNat  FromNatToInt c n arg
  | isAnnRatType     arg = solveFromNatToRat   c n arg
  | otherwise            = blockOrThrowErrors c [arg] [tcError]
  where tcError = FailedNatLitConstraint (constraintContext c) n arg
solveHasNatLits _ c _ = malformedConstraintError c

type HasFromNatSolver =
  forall m . TCM m
  => Constraint
  -> Int
  -> CheckedType
  -> m TypeClassProgress

solveSimpleFromNat :: FromNatDomain -> HasFromNatSolver
solveSimpleFromNat dom c n _arg = do
  let p = provenanceOf c
  let solution = Builtin p (FromNat n dom)
  return $ Right ([], solution)

solveFromNatToIndex :: HasFromNatSolver
solveFromNatToIndex c n arg = do
  let p = provenanceOf c
  (indexEq, index) <- unifyWithIndexType c arg
  let solution = App p (Builtin p (FromNat n FromNatToIndex)) [ImplicitArg p index]
  return $ Right ([indexEq], solution)

solveFromNatToRat :: HasFromNatSolver
solveFromNatToRat c n arg = do
  let p           = provenanceOf c
  let lin         = LinearityExpr (provenanceOf c) Constant
  let ratEq       = unify c arg (AnnRatType p lin)
  let solution    = Builtin p (FromNat n FromNatToRat)
  return $ Right ([ratEq], solution)

--------------------------------------------------------------------------------
-- HasRatLits

solveHasRatLits :: TypeClassSolver
solveHasRatLits c [arg]
  | isMeta arg           = blockOnMetas [arg]
  | isAnnRatType     arg = solveFromRatToRat c arg
  | otherwise            = blockOrThrowErrors c [arg] [tcError]
  where tcError = FailedRatLitConstraint (constraintContext c) arg
solveHasRatLits c _ = malformedConstraintError c

solveFromRatToRat :: TCM m
                  => Constraint
                  -> CheckedType
                  -> m TypeClassProgress
solveFromRatToRat c arg = do
  let p          = provenanceOf c
  let lin        = LinearityExpr (provenanceOf c) Constant
  let constraint = unify c arg (AnnRatType p lin)
  let solution   = Builtin p (FromRat FromRatToRat)
  return $ Right ([constraint], solution)

--------------------------------------------------------------------------------
-- HasConLits

solveHasVecLits :: Int -> TypeClassSolver
solveHasVecLits n c [tElem, tCont] = case tCont of
  (exprHead -> Meta{}) -> blockOnMetas [tCont]

  ListType _ tListElem -> do
    let p = provenanceOf c
    let elemEq   = unify c tElem tListElem
    let solution = BuiltinExpr p (FromVec n FromVecToList) [ImplicitArg p tListElem]
    return $ Right ([elemEq], solution)

  VectorType _ tVecElem dim -> do
    let p = provenanceOf c
    let elemEq = unify c tElem tVecElem
    let dimEq  = unify c dim (NatLiteral p n)
    let solution = BuiltinExpr p (FromVec n FromVecToVec) [ImplicitArg p tVecElem]
    return $ Right ([elemEq, dimEq], solution)

  _ -> blockOrThrowErrors c [tCont] [tcError]
    where tcError = FailedConLitConstraint (constraintContext c) tCont

solveHasVecLits _ c _ = malformedConstraintError c


--------------------------------------------------------------------------------
-- AlmostEqual

solveAlmostEqual :: TypeClassSolver
solveAlmostEqual c [targetType, subTypesExpr]
  | allOf types isMeta  = blockOnMetas types
  | otherwise           = irrelevant c <$> checkSubtypes c targetType subTypes
  where
    subTypes = getConcreteList subTypesExpr
    types    = targetType : subTypes
solveAlmostEqual c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- LessThan

solveInDomain :: Int -> TypeClassSolver
solveInDomain n c [arg] = case arg of
  (exprHead -> Meta{}) -> blockOnMetas [arg]

  IndexType _ size -> case size of
    (exprHead -> Meta{}) -> blockOnMetas [size]

    (BuiltinExpr _ (TypeClassOp FromNatTC{}) (_ :| InstanceArg _ inst@Meta{} : _)) ->
      blockOnMetas [inst]

    (NatLiteral _ m)
      | m > n     -> return $ irrelevant c []
      | otherwise -> throwError $ FailedNatLitConstraintTooBig (constraintContext c) n m

    _ -> throwError $ FailedNatLitConstraintUnknown (constraintContext c) n size

  NatType{}    -> return $ Right ([], UnitLiteral p)
  IntType{}    -> return $ Right ([], UnitLiteral p)
  AnnRatType{} -> return $ Right ([], UnitLiteral p)
  _            -> malformedConstraintError c
  where p = provenanceOf c

solveInDomain _ c _ = malformedConstraintError c


--------------------------------------------------------------------------------
-- Subtyping

type SubtypingCheck m
  = TCM m
  => Constraint
  -> CheckedType
  -> [CheckedType]
  -> m [Constraint]

checkSubtypes :: SubtypingCheck m
checkSubtypes c targetType subTypes
  | anyOf types isAnnRatType  = checkRatSubtypes    c targetType subTypes
  | anyOf types isAnnBoolType = checkBoolSubtypes   c targetType subTypes
  | anyOf types isListType    = checkListSubtypes   c targetType subTypes
  | anyOf types isVectorType  = checkVectorSubtypes c targetType subTypes
  | otherwise                 = checkSimpleSubtypes c targetType subTypes
  where types = targetType : subTypes

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
  let subElemTypeSeq = mkList p (TypeUniverse p 0) subElemTypes
  (_, recEq) <- createTC c AlmostEqualConstraint [targetElemType, subElemTypeSeq]
  return (targetEqConstraint : recEq : subEqConstraints)

checkVectorSubtypes :: SubtypingCheck m
checkVectorSubtypes c targetType subTypes = do
  let p = provenanceOf c
  dim <- freshDimMeta c
  (targetEqConstraint, targetElemType) <- unifyWithVectorType c dim targetType
  (subEqConstraints, subElemTypes) <- unzip <$> forM subTypes (unifyWithVectorType c dim)
  let subElemTypeSeq = mkList p (TypeUniverse p 0) subElemTypes
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

checkBoolTypesEqualUpTo :: TCM m
                        => Constraint
                        -> CheckedType
                        -> [CheckedType]
                        -> LinearityTypeClass
                        -> PolarityTypeClass
                        -> m [Constraint]
checkBoolTypesEqualUpTo c targetType subTypes linTC polTC = do
  (targetEqConstraint, targetLin, targetPol) <-
    unifyWithAnnBoolType c targetType

  (subEqConstraints, subLins, subPols) <-
    unzip3 <$> forM subTypes (unifyWithAnnBoolType c)

  let polCombine = combineAuxiliaryConstraints (PolarityTypeClass  polTC) (Polarity Unquantified)
  let linCombine = combineAuxiliaryConstraints (LinearityTypeClass linTC) (Linearity Constant)
  linTCConstraints <- linCombine freshLinearityMeta c targetLin subLins
  polTCConstraints <- polCombine freshPolarityMeta c targetPol subPols

  return $ targetEqConstraint : subEqConstraints <> linTCConstraints <> polTCConstraints

checkRatTypesEqualUpTo :: TCM m
                       => Constraint
                       -> CheckedType
                       -> [CheckedType]
                       -> LinearityTypeClass
                       -> m [Constraint]
checkRatTypesEqualUpTo c targetType subTypes linTC = do
  (targetEqConstraint, targetLin) <-
    unifyWithAnnRatType c targetType

  (subEqConstraints, subLins) <-
    unzip <$> forM subTypes (unifyWithAnnRatType c)

  let linCombine = combineAuxiliaryConstraints (LinearityTypeClass linTC) (Linearity Constant)
  linTCConstraints <- linCombine freshLinearityMeta c targetLin subLins

  return $ targetEqConstraint : subEqConstraints <> linTCConstraints


checkOp2SimpleTypesEqual :: Constraint
                         -> CheckedType -> CheckedType -> CheckedType
                         -> [Constraint]
checkOp2SimpleTypesEqual c arg1 arg2 res = do
  let argsEq = unify c arg1 arg2
  let resEq  = unify c arg1 res
  [argsEq, resEq]

createTC :: TCM m
         => Constraint
         -> TypeClass
         -> NonEmpty CheckedType
         -> m (Meta, Constraint)
createTC c tc argExprs = do
  let p = provenanceOf c
  let ctx = copyContext (constraintContext c)
  let args = ExplicitArg p <$> argExprs
  m <- freshTypeClassPlacementMeta p (BuiltinTypeClass p tc args)
  return (m, TC ctx (Has m tc args))

unifyWithAnnBoolType :: TCM m
                     => Constraint
                     -> CheckedType
                     -> m (Constraint, CheckedExpr, CheckedExpr)
unifyWithAnnBoolType c t = do
  let p = provenanceOf c
  lin <- freshLinearityMeta p
  pol <- freshPolarityMeta p
  let eq = unify c t (AnnBoolType p lin pol)
  return (eq, lin, pol)

unifyWithIndexType :: TCM m
                   => Constraint
                   -> CheckedType
                   -> m (Constraint, CheckedExpr)
unifyWithIndexType c t = do
  let p = provenanceOf c
  indexSize <- freshExprMeta p (NatType p) (boundContext $ constraintContext  c)
  let eq = unify c t (IndexType p indexSize)
  return (eq, indexSize)

unifyWithAnnRatType :: TCM m
                    => Constraint
                    -> CheckedType
                    -> m (Constraint, CheckedExpr)
unifyWithAnnRatType c t = do
  let p = provenanceOf c
  lin <- freshLinearityMeta p
  let eq = unify c t (AnnRatType p lin)
  return (eq, lin)

unifyWithListType :: TCM m
                 => Constraint
                 -> CheckedType
                 -> m (Constraint, CheckedExpr)
unifyWithListType c t = do
  let p = provenanceOf c
  elemType <- freshExprMeta p (TypeUniverse p 0) (boundContext $ constraintContext c)
  let eq = unify c t (ListType p elemType)
  return (eq, elemType)

unifyWithVectorType :: TCM m
                    => Constraint
                    -> CheckedExpr
                    -> CheckedType
                    -> m (Constraint, CheckedType)
unifyWithVectorType c dim t = do
  let p = provenanceOf c
  elemType <- freshExprMeta p (TypeUniverse p 0) (boundContext $ constraintContext  c)
  let eq = unify c t (VectorType p elemType dim)
  return (eq, elemType)

freshDimMeta :: TCM m => Constraint -> m CheckedExpr
freshDimMeta c = do
  let p = provenanceOf c
  freshExprMeta p (NatType p) (boundContext $ constraintContext c)

solveSimpleComparisonOp :: TCM m
                        => Constraint
                        -> CheckedType
                        -> CheckedType
                        -> CheckedType
                        -> Builtin
                        -> m TypeClassProgress
solveSimpleComparisonOp c arg1 arg2 res solution = do
  let p = provenanceOf c
  let resEq = unify c res (AnnBoolType p (LinearityExpr p Constant) (PolarityExpr p Unquantified))
  let argEq = unify c arg1 arg2
  return $ Right ([argEq, resEq], Builtin p solution)

solveIndexComparisonOp :: TCM m
                       => Constraint
                       -> CheckedType
                       -> CheckedType
                       -> CheckedType
                       -> Builtin
                       -> m TypeClassProgress
solveIndexComparisonOp c arg1 arg2 res solution = do
  let p = provenanceOf c
  (arg1Eq, _size1) <- unifyWithIndexType c arg1
  (arg2Eq, _size2) <- unifyWithIndexType c arg2
  let resEq = unify c res (AnnBoolType p (LinearityExpr p Constant) (PolarityExpr p Unquantified))
  return $ Right ([arg1Eq, arg2Eq, resEq], Builtin p solution)

solveRatComparisonOp :: TCM m
                     => Constraint
                     -> CheckedType
                     -> CheckedType
                     -> CheckedType
                     -> Builtin
                     -> m TypeClassProgress
solveRatComparisonOp c arg1 arg2 res op = do
  let p = provenanceOf c

  (arg1Eq, arg1Lin)       <- unifyWithAnnRatType  c arg1
  (arg2Eq, arg2Lin)       <- unifyWithAnnRatType  c arg2
  (resEq, resLin, resPol) <- unifyWithAnnBoolType c res

  -- The resulting linearity is the max of the linearity of the arguments.
  (_meta, linTC) <- createTC c (LinearityTypeClass MaxLinearity) [arg1Lin, arg2Lin, resLin]
  -- The polarity is unquantified.
  let polEq = unify c resPol (PolarityExpr p Unquantified)

  return $ Right ([arg1Eq, arg2Eq, resEq, linTC, polEq], Builtin p op)


combineAuxiliaryConstraints :: forall m . TCM m
                            => TypeClass
                            -> BuiltinConstructor
                            -> (Provenance -> m CheckedExpr)
                            -> Constraint
                            -> CheckedExpr
                            -> [CheckedExpr]
                            -> m [Constraint]
combineAuxiliaryConstraints tc unit makeMeta c result auxs = do
  (res, tcConstraints) <- foldPairs auxs
  let resEq = unify c res result
  return $ resEq : tcConstraints
  where
    foldPairs :: [CheckedExpr] -> m (CheckedExpr, [Constraint])
    foldPairs []       = return (Builtin mempty (Constructor unit), [])
    foldPairs [a]      = return (a, [])
    foldPairs (a : cs) = do
      (b, constraints) <- foldPairs cs
      res <- makeMeta (provenanceOf c)
      (_, tc1) <- createTC c tc [a, b, res]
      return (res, tc1 : constraints)

irrelevant :: HasProvenance a => a -> [Constraint] -> TypeClassProgress
irrelevant c newConstraints = Right (newConstraints, UnitLiteral (provenanceOf c))

blockOnMetas :: TCM m => [CheckedExpr] -> m TypeClassProgress
blockOnMetas args = do
  let metas = mapMaybe getMeta args
  progress <- blockOn metas
  return $ castProgress mempty progress

blockOrThrowErrors :: TCM m
                  => Constraint
                  -> [CheckedExpr]
                  -> [CompileError]
                  -> m TypeClassProgress
blockOrThrowErrors c args err =
  castProgress (provenanceOf c) <$> blockOnReductionBlockingMetasOrThrowError args (head err)
