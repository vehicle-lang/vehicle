module Vehicle.Compile.Type.Constraint.PolaritySolver
  ( solvePolarityConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.Normalised (NormType, getMeta, pattern VPolarityExpr)

solvePolarityConstraint ::
  TCM m =>
  PolarityTypeClass ->
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solvePolarityConstraint = \case
  NegPolarity -> solveNegPolarity
  AddPolarity q -> solveAddPolarity q
  EqPolarity eq -> solveEqPolarity eq
  ImpliesPolarity -> solveImplPolarity
  MaxPolarity -> solveMaxPolarity
  FunctionPolarity position -> solveFunctionPolarity position
  IfCondPolarity -> solveIfCondPolarity

--------------------------------------------------------------------------------
-- Operations over polarities

negPolarity ::
  (PolarityProvenance -> PolarityProvenance) ->
  Polarity ->
  Polarity
negPolarity modProv pol =
  case pol of
    Unquantified -> Unquantified
    Quantified q pp -> Quantified (neg q) (modProv pp)
    MixedParallel pp1 pp2 -> MixedParallel (modProv pp2) (modProv pp1)
    -- We don't negate a mixed sequential polarity as its the top of the polarity
    -- lattice and we want to give as meaningful and localised error messages
    -- as possible.
    MixedSequential {} -> pol

negatePolarity ::
  Provenance ->
  Polarity ->
  Polarity
negatePolarity p = negPolarity (NegateProvenance p)

addPolarity :: Provenance -> Quantifier -> Polarity -> Polarity
addPolarity p q pol = case pol of
  Unquantified -> Quantified q (QuantifierProvenance p)
  Quantified q' pp -> if q == q' then pol else MixedSequential q p pp
  MixedParallel pp1 pp2 -> MixedSequential q p (if q == Forall then pp2 else pp1)
  MixedSequential {} -> pol

maxPolarity :: Polarity -> Polarity -> Polarity
maxPolarity pol1 pol2 = case (pol1, pol2) of
  (Unquantified, _) -> pol2
  (_, Unquantified) -> pol1
  (Quantified q1 pp1, Quantified q2 pp2)
    | q1 == q2 -> pol1
    | q1 == Forall -> MixedParallel pp1 pp2
    | otherwise -> MixedParallel pp2 pp1
  (Quantified {}, MixedParallel {}) -> pol2
  (MixedParallel {}, Quantified {}) -> pol1
  (MixedParallel {}, MixedParallel {}) -> pol1
  (MixedSequential {}, _) -> pol1
  (_, MixedSequential {}) -> pol2

eqPolarity ::
  EqualityOp ->
  Provenance ->
  Polarity ->
  Polarity ->
  Polarity
eqPolarity eq p pol1 pol2 =
  let negPol = negPolarity (\pp -> EqProvenance p pp eq)
   in -- `a == b` = (a and b) or (not a and not b)
      maxPolarity
        (maxPolarity pol1 pol2)
        (maxPolarity (negPol pol1) (negPol pol2))

implPolarity ::
  Provenance ->
  Polarity ->
  Polarity ->
  Polarity
implPolarity p pol1 pol2 =
  let negPol = negPolarity (LHSImpliesProvenance p)
   in -- `a => b` = not a or b
      maxPolarity (negPol pol1) pol2

--------------------------------------------------------------------------------
-- Constraint solving

solveNegPolarity ::
  TCM m =>
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveNegPolarity c [arg1, res] = case arg1 of
  (getMeta -> Just m) -> blockOn [m]
  VPolarityExpr p pol -> do
    let ctx = contextOf c
    let resPol = VPolarityExpr p $ negatePolarity (provenanceOf ctx) pol
    return $ Progress [unify ctx res resPol]
  _ -> malformedConstraintError c
solveNegPolarity c _ = malformedConstraintError c

solveAddPolarity ::
  TCM m =>
  Quantifier ->
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveAddPolarity q c [arg1, res] = case arg1 of
  (getMeta -> Just m) -> blockOn [m]
  VPolarityExpr _ pol -> do
    let ctx = contextOf c
    let p = provenanceOf ctx
    let resPol = VPolarityExpr p $ addPolarity p q pol
    return $ Progress [unify ctx res resPol]
  _ -> malformedConstraintError c
solveAddPolarity _ c _ = malformedConstraintError c

solveMaxPolarity ::
  TCM m =>
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveMaxPolarity c [arg1, arg2, res] = case (arg1, arg2) of
  (VPolarityExpr p pol1, VPolarityExpr _ pol2) -> do
    let ctx = contextOf c
    let pol3 = VPolarityExpr p $ maxPolarity pol1 pol2
    return $ Progress [unify ctx res pol3]
  (_, VPolarityExpr _ Unquantified) ->
    return $ Progress [unify (contextOf c) arg1 res]
  (VPolarityExpr _ Unquantified, _) ->
    return $ Progress [unify (contextOf c) arg2 res]
  (getMeta -> Just m1, _) -> blockOn [m1]
  (_, getMeta -> Just m2) -> blockOn [m2]
  _ -> malformedConstraintError c
solveMaxPolarity c _ = malformedConstraintError c

solveEqPolarity ::
  TCM m =>
  EqualityOp ->
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveEqPolarity eq c [arg1, arg2, res] = case (arg1, arg2) of
  (VPolarityExpr p pol1, VPolarityExpr _ pol2) -> do
    let ctx = contextOf c
    let pol3 = VPolarityExpr p $ eqPolarity eq (provenanceOf ctx) pol1 pol2
    return $ Progress [unify ctx res pol3]
  (getMeta -> Just m1, _) -> blockOn [m1]
  (_, getMeta -> Just m2) -> blockOn [m2]
  _ -> malformedConstraintError c
solveEqPolarity _ c _ = malformedConstraintError c

solveImplPolarity ::
  TCM m =>
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveImplPolarity c [arg1, arg2, res] = case (arg1, arg2) of
  (VPolarityExpr p pol1, VPolarityExpr _ pol2) -> do
    let ctx = contextOf c
    let pol3 = VPolarityExpr p $ implPolarity (provenanceOf ctx) pol1 pol2
    return $ Progress [unify ctx res pol3]
  (getMeta -> Just m1, _) -> blockOn [m1]
  (_, getMeta -> Just m2) -> blockOn [m2]
  _ -> malformedConstraintError c
solveImplPolarity c _ = malformedConstraintError c

solveFunctionPolarity ::
  TCM m =>
  FunctionPosition ->
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveFunctionPolarity functionPosition c [arg, res] = case arg of
  (getMeta -> Just m1) -> blockOn [m1]
  VPolarityExpr _ pol -> do
    let ctx = contextOf c
    let p = provenanceOf ctx
    let addFuncProv pp = PolFunctionProvenance p pp functionPosition
    let pol3 = VPolarityExpr p $ mapPolarityProvenance addFuncProv pol
    return $ Progress [unify ctx res pol3]
  _ -> malformedConstraintError c
solveFunctionPolarity _ c _ = malformedConstraintError c

solveIfCondPolarity ::
  TCM m =>
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveIfCondPolarity c [arg] = case arg of
  (getMeta -> Just m1) -> blockOn [m1]
  VPolarityExpr _ pol -> case pol of
    Unquantified -> return $ Progress []
    _ -> throwError $ QuantifiedIfCondition (contextOf c)
  _ -> malformedConstraintError c
solveIfCondPolarity c _ = malformedConstraintError c
