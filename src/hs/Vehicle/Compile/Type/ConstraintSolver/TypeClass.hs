module Vehicle.Compile.Type.ConstraintSolver.TypeClass
  ( solveTypeClassConstraint
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Control.Monad.Except ( throwError )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.ConstraintSolver.Polarity
import Vehicle.Compile.Type.ConstraintSolver.Linearity
import Vehicle.Compile.Type.ConstraintSolver.Core
import Vehicle.Compile.Type.WeakHeadNormalForm (whnfExprWithMetas)
import Vehicle.Language.StandardLibrary.Names

--------------------------------------------------------------------------------
-- Public interface

solveTypeClassConstraint :: MonadMeta m
                         => ConstraintContext
                         -> TypeClassConstraint
                         -> m ConstraintProgress
solveTypeClassConstraint ctx c@(Has m tc args) = do
  progress <- solve tc (TC ctx c) (onlyExplicit args)

  case progress of
    Left  metas                      -> return $ Stuck metas
    Right (newConstraints, solution) -> do
      metaSolved m solution
      return $ Progress newConstraints

--------------------------------------------------------------------------------
-- Solver

type TypeClassProgress = Either MetaSet ([Constraint], CheckedExpr)

solve :: MonadMeta m
      => TypeClass
      -> Constraint
      -> [CheckedType]
      -> m TypeClassProgress
solve = \case
  HasEq eq            -> solveHasEq eq
  HasOrd ord          -> solveHasOrd ord
  HasNot              -> solveHasNot
  HasAnd              -> solveHasAnd
  HasOr               -> solveHasOr
  HasImplies          -> solveHasImplies
  HasQuantifier q     -> solveHasQuantifier q
  HasNeg              -> solveHasNeg
  HasAdd              -> solveHasAdd
  HasSub              -> solveHasSub
  HasMul              -> solveHasMul
  HasDiv              -> solveHasDiv
  HasFold             -> solveHasFold
  HasQuantifierIn q   -> solveHasQuantifierIn q

  HasNatLits n -> solveHasNatLits n
  HasRatLits   -> solveHasRatLits
  HasVecLits n -> solveHasVecLits n

  AlmostEqualConstraint   -> solveAlmostEqual
  NatInDomainConstraint n -> solveInDomain n

  MaxLinearity -> castProgressFn solveMaxLinearity
  MulLinearity -> castProgressFn solveMulLinearity

  NegPolarity     -> castProgressFn solveNegPolarity
  AddPolarity q   -> castProgressFn $ solveAddPolarity q
  EqPolarity eq   -> castProgressFn $ solveEqPolarity eq
  ImpliesPolarity -> castProgressFn solveImplPolarity
  MaxPolarity     -> castProgressFn solveMaxPolarity

-- A temporary hack until we separate out the solvers properly.
castProgressFn :: MonadMeta m
               => (Constraint -> [CheckedType] -> m ConstraintProgress)
               -> (Constraint -> [CheckedType] -> m TypeClassProgress)
castProgressFn f c e = castProgress (provenanceOf c) <$> f c e

castProgress :: Provenance -> ConstraintProgress -> TypeClassProgress
castProgress c = \case
  Stuck metas             -> Left metas
  Progress newConstraints -> irrelevant c newConstraints

--------------------------------------------------------------------------------
-- HasEq

solveHasEq :: MonadMeta m
           => EqualityOp
           -> Constraint
           -> [CheckedType]
           -> m TypeClassProgress
solveHasEq op c [arg1, arg2, res]
  | allOf args isMeta        = blockOnMetas args
  | anyOf args isIndexType   = solveIndexComparisonOp  c arg1 arg2 res (Equals EqIndex op)
  | anyOf args isNatType     = solveSimpleComparisonOp c arg1 arg2 res (Equals EqNat op)
  | anyOf args isIntType     = solveSimpleComparisonOp c arg1 arg2 res (Equals EqInt op)
  | anyOf args isAnnRatType  = solveRatComparisonOp    c arg1 arg2 res (Equals EqRat op)
  | anyOf args isAnnBoolType = solveBoolEquals         c arg1 arg2 res op
  | anyOf args isVectorType  = solveVectorEquals op c arg1 arg2 res
  | otherwise                = blockOrThrowError c args tcError
  where
    args = [arg1, arg2]
    allowedTypes = allowed [Bool, Index, Nat, Int, Rat, List, Vector, Tensor]
    tcError = head $
      tcArgError c arg1 (EqualsTC op) allowedTypes 1 2 <>
      tcArgError c arg2 (EqualsTC op) allowedTypes 2 2


solveHasEq _ c _ = malformedConstraintError c

solveBoolEquals :: MonadMeta m
                => Constraint
                -> CheckedType
                -> CheckedType
                -> CheckedType
                -> EqualityOp
                -> m TypeClassProgress
solveBoolEquals c arg1 arg2 res op = do
  let p = provenanceOf c
  constraints <- checkBoolTypesEqual c res [arg1, arg2] MaxLinearity (EqPolarity op)
  let solution = FreeVar p (identifierOf StdEqualsBool)
  return $ Right (constraints, solution)

{-
solveListEquals :: MonadMeta m
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

solveVectorEquals :: MonadMeta m
                  => EqualityOp
                  -> Constraint
                  -> CheckedType
                  -> CheckedType
                  -> CheckedType
                  -> m TypeClassProgress
solveVectorEquals op c arg1 arg2 res = do
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

solveHasOrd :: MonadMeta m
            => OrderOp
            -> Constraint
            -> [CheckedType]
            -> m TypeClassProgress
solveHasOrd op c [arg1, arg2, res]
  | allOf args isMeta           = blockOnMetas args
  | anyOf args isIndexType      = solveIndexComparisonOp  c arg1 arg2 res (Order OrderIndex op)
  | anyOf args isNatType        = solveSimpleComparisonOp c arg1 arg2 res (Order OrderNat   op)
  | anyOf args isIntType        = solveSimpleComparisonOp c arg1 arg2 res (Order OrderInt   op)
  | anyOf args isAnnRatType     = solveRatComparisonOp    c arg1 arg2 res (Order OrderRat   op)
  | otherwise                   = blockOrThrowError c args tcError
  where
    args         = [arg1, arg2]
    allowedTypes = allowed [Index, Nat, Int, Rat]
    tcError      = head $
      tcArgError c arg1 (OrderTC op) allowedTypes 1 2 <>
      tcArgError c arg2 (OrderTC op) allowedTypes 1 2
solveHasOrd _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasNot

solveHasNot :: MonadMeta m
            => Constraint
            -> [CheckedType]
            -> m TypeClassProgress
solveHasNot c [arg, res] = do
  let p = provenanceOf c
  (argEq, argLin, argPol) <- unifyWithAnnBoolType c arg
  (resEq, resLin, resPol) <- unifyWithAnnBoolType c res

  let linEq = unify c argLin resLin
  (_, polTC) <- createTC c NegPolarity [argPol, resPol]
  let solution = Builtin p Not

  return $ Right ([argEq, resEq, linEq, polTC], solution)

solveHasNot c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasAndOr

solveHasBoolOp2 :: MonadMeta m
                => TypeClass
                -> Builtin
                -> Constraint
                -> [CheckedType]
                -> m TypeClassProgress
solveHasBoolOp2 polConstraint solutionBuiltin c [arg1, arg2, res] = do
  let p = provenanceOf c
  constraints <- checkBoolTypesEqual c res [arg1, arg2] MaxLinearity polConstraint
  let solution = Builtin p solutionBuiltin
  return $ Right (constraints, solution)
solveHasBoolOp2 _ _ c _ = malformedConstraintError c

solveHasAnd :: MonadMeta m
            => Constraint
            -> [CheckedType]
            -> m TypeClassProgress
solveHasAnd = solveHasBoolOp2 MaxPolarity And

solveHasOr :: MonadMeta m
            => Constraint
            -> [CheckedType]
            -> m TypeClassProgress
solveHasOr = solveHasBoolOp2 MaxPolarity Or

solveHasImplies :: MonadMeta m
                => Constraint
                -> [CheckedType]
                -> m TypeClassProgress
solveHasImplies = solveHasBoolOp2 ImpliesPolarity Implies

--------------------------------------------------------------------------------
-- HasQuantifier

solveHasQuantifier :: forall m . MonadMeta m
                   => Quantifier
                   -> Constraint
                   -> [CheckedType]
                   -> m TypeClassProgress
solveHasQuantifier q c [domain, body, res]
  | isMeta        domain = blockOnMetas [domain]
  | isAnnBoolType domain = solveBoolQuantifier   q c domain body res
  | isIndexType   domain = solveIndexQuantifier  q c domain body res
  | isNatType     domain = solveSimpleQuantifier q c domain body res PostulateForallNat PostulateForallNat
  | isIntType     domain = solveSimpleQuantifier q c domain body res PostulateForallInt PostulateForallInt
  | isAnnRatType  domain = solveRatQuantifier    q c domain body res
  | isVectorType  domain = solveVectorQuantifier q c domain body res
  | otherwise            = blockOrThrowError c [domain] tcError
    where tcError = FailedQuantifierConstraintDomain (constraintContext c) domain q

solveHasQuantifier _ c _ = malformedConstraintError c

solveBoolQuantifier :: MonadMeta m
                    => Quantifier
                    -> Constraint
                    -> CheckedType
                    -> CheckedType
                    -> CheckedType
                    -> m TypeClassProgress
solveBoolQuantifier q c domain body res = do
  let p = provenanceOf c

  -- If we're quantifying over a Bool then it itself is linear and unquantified
  let domainLin = LinearityExpr p Constant
  let domainPol = PolarityExpr p Unquantified
  let domainEq = unify c domain (AnnBoolType p domainLin domainPol)

  -- The body is some unknown annotated boolean
  (bodyEq, _, _) <- unifyWithAnnBoolType c body

  -- The result is equal to the body
  let resEq = unify c res body

  let solution = FreeVar p $ identifierOf $ case q of
        Forall -> StdForallBool
        Exists -> StdExistsBool

  return $ Right ([domainEq, bodyEq, resEq], solution)

solveIndexQuantifier :: MonadMeta m
                     => Quantifier
                     -> Constraint
                     -> CheckedType
                     -> CheckedType
                     -> CheckedType
                     -> m TypeClassProgress
solveIndexQuantifier q c domain body res = do
  let p = provenanceOf c

  (domainEq, indexSize) <- unifyWithIndexType c domain
  (bodyEq, _, _)        <- unifyWithAnnBoolType c body
  let resEq          = unify c res body

  let method = identifierOf $ if q == Forall then StdForallIndex else StdExistsIndex
  let solution = App p (FreeVar (provenanceOf c) method)
        [ ImplicitArg p indexSize
        ]

  return $ Right ([domainEq, bodyEq, resEq], solution)

solveSimpleQuantifier :: MonadMeta m
                      => Quantifier
                      -> Constraint
                      -> CheckedType
                      -> CheckedType
                      -> CheckedType
                      -> Identifier
                      -> Identifier
                      -> m TypeClassProgress
solveSimpleQuantifier q c _domain body res forallMethod existsMethod = do
  (bodyEq, _, _) <- unifyWithAnnBoolType c body
  let resEq          = unify c res body

  let solution = FreeVar (provenanceOf c) $ case q of
        Forall -> forallMethod
        Exists -> existsMethod

  return $ Right ([bodyEq, resEq], solution)

solveRatQuantifier :: MonadMeta m
                   => Quantifier
                   -> Constraint
                   -> CheckedType
                   -> CheckedType
                   -> CheckedType
                   -> m TypeClassProgress
solveRatQuantifier q c domain body res = do
  let p = provenanceOf c

  -- The rational being quantified over is, by definition, linear
  let domainLin = LinearityExpr p (Linear (QuantifiedVariableProvenance p))
  let domainEq  = unify c domain (AnnRatType p domainLin)

  -- The body must be of some Bool type
  (bodyEq, bodyLin, bodyPol) <- unifyWithAnnBoolType c body

  -- Generate a new polarity for the result type and relate it to the polarity of the body.
  resPol     <- freshPolarityMeta p
  (_, polTC) <- createTC c (AddPolarity q) [bodyPol, resPol]

  -- The result type is the Bool type with the same linearity as the body.
  let resEq  = unify c res  (AnnBoolType p bodyLin resPol)

  let solution = FreeVar p $ case q of
        Forall -> PostulateForallRat
        Exists -> PostulateExistsRat

  return $ Right ([domainEq, polTC, bodyEq, resEq], solution)

solveVectorQuantifier :: MonadMeta m
                      => Quantifier
                      -> Constraint
                      -> CheckedType
                      -> CheckedType
                      -> CheckedType
                      -> m TypeClassProgress
solveVectorQuantifier q c domain body res = do
  let p = provenanceOf c
  dim <- freshDimMeta c
  (domainEq, vecElem) <- unifyWithVectorType c dim domain

  -- Recursively check that you can quantify over it.
  (meta, recTC) <- createTC c (HasQuantifier q) [vecElem, body, res]

  let method = if q == Forall then StdForallVector else StdExistsVector
  let solution = App p (FreeVar (provenanceOf c) (identifierOf method))
        [ ImplicitArg p vecElem
        , ImplicitArg p dim
        , InstanceArg p (Meta p meta)
        ]

  return $ Right ([domainEq, recTC], solution)

--------------------------------------------------------------------------------
-- HasNeg

solveHasNeg :: MonadMeta m
            => Constraint
            -> [CheckedType]
            -> m TypeClassProgress
solveHasNeg c [arg, res]
  | allOf types isMeta       = blockOnMetas [arg, res]
  | anyOf types isIntType    = solveNeg c res arg NegInt
  | anyOf types isAnnRatType = solveNeg c res arg NegRat
  | otherwise                = blockOrThrowError c types tcError
  where
    types = [arg, res]
    allowedTypes = allowed [Int, Rat]
    tcError = head $
      tcArgError    c arg NegTC allowedTypes 1 1 <>
      tcResultError c res NegTC allowedTypes

solveHasNeg c _ = malformedConstraintError c

solveNeg :: MonadMeta m
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

solveHasAdd :: MonadMeta m
            => Constraint
            -> [CheckedType]
            -> m TypeClassProgress
solveHasAdd c types@[arg1, arg2, res]
  | allOf types isMeta           = blockOnMetas types
  | anyOf types isNatType        = solveAddNat    c arg1 arg2 res
  | anyOf types isIntType        = solveAddInt    c arg1 arg2 res
  | anyOf types isAnnRatType     = solveAddRat    c arg1 arg2 res
  | anyOf types isVectorType     = solveAddVector c arg1 arg2 res
  | otherwise                    = blockOrThrowError c types tcError
  where
    allowedTypes = allowed [Nat, Int, Rat]
    tcError  = head $
      tcArgError    c arg1 AddTC allowedTypes 1 2 <>
      tcArgError    c arg2 AddTC allowedTypes 2 2 <>
      tcResultError c res  AddTC allowedTypes

solveHasAdd c _ = malformedConstraintError c

solveAddNat :: MonadMeta m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveAddNat c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Add AddNat)
  return $ Right (constraints, solution)

solveAddInt :: MonadMeta m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveAddInt c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Add AddInt)
  return $ Right (constraints, solution)

solveAddRat :: MonadMeta m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveAddRat c arg1 arg2 res = do
  let p = provenanceOf c
  constraints <- checkRatTypesEqual c res [arg1, arg2] MaxLinearity
  let solution = Builtin p (Add AddRat)
  return $ Right (constraints, solution)

solveAddVector :: MonadMeta m
               => Constraint
               -> CheckedType
               -> CheckedType
               -> CheckedType
               -> m TypeClassProgress
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

solveHasSub :: MonadMeta m
            => Constraint
            -> [CheckedType]
            -> m TypeClassProgress
solveHasSub c types@[arg1, arg2, res]
  | allOf types isMeta           = blockOnMetas types
  | anyOf types isIntType        = solveSubInt    c arg1 arg2 res
  | anyOf types isAnnRatType     = solveSubRat    c arg1 arg2 res
  | anyOf types isVectorType     = solveSubVector c arg1 arg2 res
  | otherwise                    = blockOrThrowError c types tcError
  where
    allowedTypes = allowed [Int, Rat]
    tcError  = head $
      tcArgError    c arg1 SubTC allowedTypes 1 2 <>
      tcArgError    c arg2 SubTC allowedTypes 2 2 <>
      tcResultError c res  SubTC allowedTypes

solveHasSub c _ = malformedConstraintError c

solveSubInt :: MonadMeta m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveSubInt c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Sub SubInt)
  return $ Right (constraints, solution)

solveSubRat :: MonadMeta m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveSubRat c arg1 arg2 res = do
  let p = provenanceOf c
  constraints <- checkRatTypesEqual c res [arg1, arg2] MaxLinearity
  let solution = Builtin p (Sub SubRat)
  return $ Right (constraints, solution)

solveSubVector :: MonadMeta m
               => Constraint
               -> CheckedType
               -> CheckedType
               -> CheckedType
               -> m TypeClassProgress
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

solveHasMul :: MonadMeta m
            => Constraint
            -> [CheckedType]
            -> m TypeClassProgress
solveHasMul c types@[arg1, arg2, res]
  | allOf types isMeta           = blockOnMetas types
  | anyOf types isNatType        = solveMulNat c arg1 arg2 res
  | anyOf types isIntType        = solveMulInt c arg1 arg2 res
  | anyOf types isAnnRatType     = solveMulRat c arg1 arg2 res
  | otherwise                    = blockOrThrowError c types tcError
  where
    allowedTypes = allowed [Nat, Int, Rat]
    tcError  = head $
      tcArgError    c arg1 MulTC allowedTypes 1 2 <>
      tcArgError    c arg2 MulTC allowedTypes 2 2 <>
      tcResultError c res  MulTC allowedTypes
solveHasMul c _ = malformedConstraintError c

solveMulNat :: MonadMeta m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveMulNat c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Mul MulNat)
  return $ Right (constraints, solution)

solveMulInt :: MonadMeta m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveMulInt c arg1 arg2 res = do
  let p = provenanceOf c
  let constraints = checkOp2SimpleTypesEqual c arg1 arg2 res
  let solution = Builtin p (Mul MulInt)
  return $ Right (constraints, solution)

solveMulRat :: MonadMeta m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveMulRat c arg1 arg2 res = do
  let p = provenanceOf c
  constraints <- checkRatTypesEqual c res [arg1, arg2] MulLinearity
  let solution = Builtin p (Mul MulRat)
  return $ Right (constraints, solution)

--------------------------------------------------------------------------------
-- HasDiv

solveHasDiv :: MonadMeta m
            => Constraint
            -> [CheckedType]
            -> m TypeClassProgress
solveHasDiv c types@[arg1, arg2, res]
  | allOf types isMeta           = blockOnMetas types
  | anyOf types isAnnRatType     = solveRatDiv c arg1 arg2 res
  | otherwise                    = blockOrThrowError c types tcError
  where
    allowedTypes = allowed [Rat]
    tcError = head $
      tcArgError    c arg1 DivTC allowedTypes 1 2 <>
      tcArgError    c arg2 DivTC allowedTypes 2 2 <>
      tcResultError c res  DivTC allowedTypes

solveHasDiv c _ = malformedConstraintError c

solveRatDiv :: MonadMeta m
            => Constraint
            -> CheckedType
            -> CheckedType
            -> CheckedType
            -> m TypeClassProgress
solveRatDiv c arg1 arg2 res = do
  constraints <- checkRatTypesEqual c res [arg1, arg2] MulLinearity
  let solution = Builtin (provenanceOf c) (Div DivRat)
  return $ Right (constraints, solution)

--------------------------------------------------------------------------------
-- HasFold

solveHasFold :: MonadMeta m
             => Constraint
             -> [CheckedType]
             -> m TypeClassProgress
solveHasFold c [tElem, tCont] = case tCont of
  (exprHead -> Meta{})      -> blockOnMetas [tCont]
  ListType   _ tListElem    -> solveFoldList c tElem tListElem
  VectorType _ tVecElem dim -> solveFoldVec  c tElem tVecElem dim
  _                         -> blockOrThrowError c [tCont] tcError
    where tcError = FailedFoldConstraintContainer (constraintContext c) tCont

solveHasFold c _ = malformedConstraintError c

solveFoldList :: MonadMeta m
              => Constraint
              -> CheckedType
              -> CheckedType
              -> m TypeClassProgress
solveFoldList c tElem tListElem = do
  let constraint = unify c tElem tListElem
  let solution = Builtin (provenanceOf c) (Fold FoldList)
  return $ Right ([constraint], solution)

solveFoldVec :: MonadMeta m
              => Constraint
              -> CheckedType
              -> CheckedType
              -> CheckedExpr
              -> m TypeClassProgress
solveFoldVec c tElem tListElem _dim = do
  let constraint = unify c tElem tListElem
  let solution = Builtin (provenanceOf c) (Fold FoldVector)
  return $ Right ([constraint], solution)

--------------------------------------------------------------------------------
-- HasQuantifierIn

solveHasQuantifierIn :: MonadMeta m
                     => Quantifier
                     -> Constraint
                     -> [CheckedType]
                     -> m TypeClassProgress
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

  _ -> blockOrThrowError c [tCont] tcError
    where tcError = FailedQuantInConstraintContainer (constraintContext c) tCont q

solveHasQuantifierIn _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- HasNatLits

solveHasNatLits :: MonadMeta m
                => Int
                -> Constraint
                -> [CheckedType]
                -> m TypeClassProgress
solveHasNatLits n c [arg]
  | isMeta arg           = blockOnMetas [arg]
  | isIndexType      arg = solveFromNatToIndex c n arg
  | isNatType        arg = solveSimpleFromNat  c n arg FromNatToNat
  | isIntType        arg = solveSimpleFromNat  c n arg FromNatToInt
  | isAnnRatType     arg = solveFromNatToRat   c n arg
  | otherwise            = blockOrThrowError c [arg] tcError
  where tcError = FailedNatLitConstraint (constraintContext c) n arg
solveHasNatLits _ c _ = malformedConstraintError c

solveSimpleFromNat :: MonadMeta m
                   => Constraint
                   -> Int
                   -> CheckedType
                   -> FromNatDomain
                   -> m TypeClassProgress
solveSimpleFromNat c n _arg dom = do
  let p = provenanceOf c
  let solution = Builtin p (FromNat n dom)
  return $ Right ([], solution)

solveFromNatToIndex :: MonadMeta m
                    => Constraint
                    -> Int
                    -> CheckedType
                    -> m TypeClassProgress
solveFromNatToIndex c n arg = do
  let p = provenanceOf c
  (indexEq, index) <- unifyWithIndexType c arg
  let solution = App p (Builtin p (FromNat n FromNatToIndex)) [ImplicitArg p index]
  return $ Right ([indexEq], solution)

solveFromNatToRat :: MonadMeta m
                  => Constraint
                  -> Int
                  -> CheckedType
                  -> m TypeClassProgress
solveFromNatToRat c n arg = do
  let p           = provenanceOf c
  let lin         = LinearityExpr (provenanceOf c) Constant
  let ratEq       = unify c arg (AnnRatType p lin)
  let solution    = Builtin p (FromNat n FromNatToRat)
  return $ Right ([ratEq], solution)

--------------------------------------------------------------------------------
-- HasRatLits

solveHasRatLits :: MonadMeta m
               => Constraint
               -> [CheckedExpr]
               -> m TypeClassProgress
solveHasRatLits c [arg]
  | isMeta arg           = blockOnMetas [arg]
  | isAnnRatType     arg = solveFromRatToRat c arg
  | otherwise            = blockOrThrowError c [arg] tcError
  where tcError = FailedRatLitConstraint (constraintContext c) arg
solveHasRatLits c _ = malformedConstraintError c

solveFromRatToRat :: MonadMeta m
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

solveHasVecLits :: MonadMeta m
                => Int
                -> Constraint
                -> [CheckedType]
                -> m TypeClassProgress
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

  _ -> blockOrThrowError c [tCont] tcError
    where tcError = FailedConLitConstraint (constraintContext c) tCont

solveHasVecLits _ c _ = malformedConstraintError c


--------------------------------------------------------------------------------
-- AlmostEqual

solveAlmostEqual :: MonadMeta m
                 => Constraint
                 -> [CheckedType]
                 -> m TypeClassProgress
solveAlmostEqual c [targetType, subTypesExpr]
  | allOf types isMeta        = blockOnMetas types
  | anyOf types isAnnRatType  = irrelevant c <$> solveRatTypesEqual    c targetType subTypes
  | anyOf types isAnnBoolType = irrelevant c <$> solveBoolTypesEqual   c targetType subTypes
  | anyOf types isListType    = irrelevant c <$> solveListTypesEqual   c targetType subTypes
  | anyOf types isVectorType  = irrelevant c <$> solveVectorTypesEqual c targetType subTypes
  | otherwise                 = irrelevant c <$> solveSimpleTypesEqual c types
  where
    subTypes = getConcreteList subTypesExpr
    types    = targetType : subTypes
solveAlmostEqual c _ = malformedConstraintError c

solveBoolTypesEqual :: MonadMeta m
                    => Constraint
                    -> CheckedType
                    -> [CheckedType]
                    -> m [Constraint]
solveBoolTypesEqual c targetType subTypes =
  checkBoolTypesEqual c targetType subTypes MaxLinearity MaxPolarity

solveRatTypesEqual :: MonadMeta m
                   => Constraint
                   -> CheckedType
                   -> [CheckedType]
                   -> m [Constraint]
solveRatTypesEqual c targetType subTypes =
  checkRatTypesEqual c targetType subTypes MaxLinearity

solveListTypesEqual :: MonadMeta m
                    => Constraint
                    -> CheckedType
                    -> [CheckedType]
                    -> m [Constraint]
solveListTypesEqual c targetType subTypes = do
  let p = provenanceOf c
  (targetEqConstraint, targetElemType) <- unifyWithListType c targetType
  (subEqConstraints, subElemTypes) <- unzip <$> forM subTypes (unifyWithListType c)
  let subElemTypeSeq = mkList p (TypeUniverse p 0) subElemTypes
  (_, recEq) <- createTC c AlmostEqualConstraint [targetElemType, subElemTypeSeq]
  return (targetEqConstraint : recEq : subEqConstraints)

solveVectorTypesEqual :: MonadMeta m
                      => Constraint
                      -> CheckedType
                      -> [CheckedType]
                      -> m [Constraint]
solveVectorTypesEqual c targetType subTypes = do
  let p = provenanceOf c
  dim <- freshDimMeta c
  (targetEqConstraint, targetElemType) <- unifyWithVectorType c dim targetType
  (subEqConstraints, subElemTypes) <- unzip <$> forM subTypes (unifyWithVectorType c dim)
  let subElemTypeSeq = mkList p (TypeUniverse p 0) subElemTypes
  (_, recEq) <- createTC c AlmostEqualConstraint [targetElemType, subElemTypeSeq]
  return (targetEqConstraint : recEq : subEqConstraints)

solveSimpleTypesEqual :: MonadMeta m
                      => Constraint
                      -> [CheckedType]
                      -> m [Constraint]
solveSimpleTypesEqual c types = do
  let adjacentPairs = zip types $ tail types
  let constraints = map (uncurry (unify c)) adjacentPairs
  return constraints

--------------------------------------------------------------------------------
-- LessThan

solveInDomain :: MonadMeta m
              => Int
              -> Constraint
              -> [CheckedType]
              -> m TypeClassProgress
solveInDomain n c [arg] = case arg of
  (exprHead -> Meta{}) -> blockOnMetas [arg]

  IndexType _ size -> do
    -- TODO normalising here is a total hack
    normSize <- whnfExprWithMetas (variableContext c) size
    case normSize of
      (exprHead -> Meta{}) -> blockOnMetas [normSize]

      (BuiltinExpr _ (TypeClassOp FromNatTC{}) (_ :| InstanceArg _ inst@Meta{} : _)) ->
        blockOnMetas [inst]

      (NatLiteral _ m)
        | m > n     -> return $ irrelevant c []
        | otherwise -> throwError $ FailedNatLitConstraintTooBig (constraintContext c) n m

      _ -> throwError $ FailedNatLitConstraintUnknown (constraintContext c) n normSize

  NatType{}    -> return $ Right ([], UnitLiteral p)
  IntType{}    -> return $ Right ([], UnitLiteral p)
  AnnRatType{} -> return $ Right ([], UnitLiteral p)
  _         -> malformedConstraintError c
  where p = provenanceOf c

solveInDomain _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- Utilities

checkBoolTypesEqual :: MonadMeta m
                    => Constraint
                    -> CheckedType
                    -> [CheckedType]
                    -> TypeClass
                    -> TypeClass
                    -> m [Constraint]
checkBoolTypesEqual c targetType subTypes linTC polTC = do
  (targetEqConstraint, targetLin, targetPol) <-
    unifyWithAnnBoolType c targetType

  (subEqConstraints, subLins, subPols) <-
    unzip3 <$> forM subTypes (unifyWithAnnBoolType c)

  linTCConstraints <- combineAuxiliaryConstraints linTC freshLinearityMeta c targetLin subLins
  polTCConstraints <- combineAuxiliaryConstraints polTC freshPolarityMeta  c targetPol subPols

  return $ targetEqConstraint : subEqConstraints <> linTCConstraints <> polTCConstraints

checkRatTypesEqual :: MonadMeta m
                   => Constraint
                   -> CheckedType
                   -> [CheckedType]
                   -> TypeClass
                   -> m [Constraint]
checkRatTypesEqual c targetType subTypes linTC = do
  (targetEqConstraint, targetLin) <-
    unifyWithAnnRatType c targetType

  (subEqConstraints, subLins) <-
    unzip <$> forM subTypes (unifyWithAnnRatType c)

  linTCConstraints <- combineAuxiliaryConstraints linTC freshLinearityMeta c targetLin subLins

  return $ targetEqConstraint : subEqConstraints <> linTCConstraints

checkOp2SimpleTypesEqual :: Constraint
                         -> CheckedType -> CheckedType -> CheckedType
                         -> [Constraint]
checkOp2SimpleTypesEqual c arg1 arg2 res = do
  let argsEq = unify c arg1 arg2
  let resEq  = unify c arg1 res
  [argsEq, resEq]

createTC :: MonadMeta m
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

unifyWithAnnBoolType :: MonadMeta m
                     => Constraint
                     -> CheckedType
                     -> m (Constraint, CheckedExpr, CheckedExpr)
unifyWithAnnBoolType c t = do
  let p = provenanceOf c
  lin <- freshLinearityMeta p
  pol <- freshPolarityMeta p
  let eq = unify c t (AnnBoolType p lin pol)
  return (eq, lin, pol)

unifyWithIndexType :: MonadMeta m
                   => Constraint
                   -> CheckedType
                   -> m (Constraint, CheckedExpr)
unifyWithIndexType c t = do
  let p = provenanceOf c
  indexSize <- freshExprMeta p (NatType p) (boundContext c)
  let eq = unify c t (IndexType p indexSize)
  return (eq, indexSize)

unifyWithAnnRatType :: MonadMeta m
                    => Constraint
                    -> CheckedType
                    -> m (Constraint, CheckedExpr)
unifyWithAnnRatType c t = do
  let p = provenanceOf c
  lin <- freshLinearityMeta p
  let eq = unify c t (AnnRatType p lin)
  return (eq, lin)

unifyWithListType :: MonadMeta m
                 => Constraint
                 -> CheckedType
                 -> m (Constraint, CheckedExpr)
unifyWithListType c t = do
  let p = provenanceOf c
  elemType <- freshExprMeta p (TypeUniverse p 0) (boundContext c)
  let eq = unify c t (ListType p elemType)
  return (eq, elemType)

unifyWithVectorType :: MonadMeta m
                    => Constraint
                    -> CheckedExpr
                    -> CheckedType
                    -> m (Constraint, CheckedType)
unifyWithVectorType c dim t = do
  let p = provenanceOf c
  elemType <- freshExprMeta p (TypeUniverse p 0) (boundContext c)
  let eq = unify c t (VectorType p elemType dim)
  return (eq, elemType)

freshDimMeta :: MonadMeta m => Constraint -> m CheckedExpr
freshDimMeta c = do
  let p = provenanceOf c
  freshExprMeta p (NatType p) (boundContext c)

solveSimpleComparisonOp :: MonadMeta m
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

solveIndexComparisonOp :: MonadMeta m
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

solveRatComparisonOp :: MonadMeta m
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
  (_meta, linTC) <- createTC c MaxLinearity [arg1Lin, arg2Lin, resLin]
  -- The polarity is unquantified.
  let polEq = unify c resPol (PolarityExpr p Unquantified)

  return $ Right ([arg1Eq, arg2Eq, resEq, linTC, polEq], Builtin p op)


combineAuxiliaryConstraints :: forall m . MonadMeta m
                            => TypeClass
                            -> (Provenance -> m CheckedExpr)
                            -> Constraint
                            -> CheckedExpr
                            -> [CheckedExpr]
                            -> m [Constraint]
combineAuxiliaryConstraints tc makeMeta c result auxs = do
  (res, tcConstraints) <- foldPairs auxs
  let resEq = unify c res result
  return $ resEq : tcConstraints
  where
    foldPairs :: [CheckedExpr] -> m (CheckedExpr, [Constraint])
    foldPairs []       = compilerDeveloperError "Empty list of auxiliary expressions"
    foldPairs [a]      = return (a, [])
    foldPairs (a : cs) = do
      (b, constraints) <- foldPairs cs
      res <- makeMeta (provenanceOf c)
      (_, tc1) <- createTC c tc [a, b, res]
      return (res, tc1 : constraints)

irrelevant :: HasProvenance a => a -> [Constraint] -> TypeClassProgress
irrelevant c newConstraints = Right (newConstraints, UnitLiteral (provenanceOf c))

blockOnMetas :: MonadMeta m => [CheckedExpr] -> m TypeClassProgress
blockOnMetas args = do
  let metas = catMaybes (getMeta <$> args)
  progress <- blockOn metas
  return $ castProgress mempty progress

blockOrThrowError :: MonadMeta m
                  => Constraint
                  -> [CheckedExpr]
                  -> CompileError
                  -> m TypeClassProgress
blockOrThrowError c args err =
  castProgress (provenanceOf c) <$> blockOnReductionBlockingMetasOrThrowError args err