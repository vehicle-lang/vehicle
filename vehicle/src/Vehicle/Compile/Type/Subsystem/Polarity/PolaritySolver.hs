module Vehicle.Compile.Type.Subsystem.Polarity.PolaritySolver
  ( solvePolarityConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Substitution (substMetas)
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Polarity.Core
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised

solvePolarityConstraint ::
  (MonadPolaritySolver m) =>
  WithContext PolarityTypeClassConstraint ->
  m ()
solvePolarityConstraint (WithContext constraint ctx) = do
  normConstraint@(Has _ expr) <- substMetas constraint
  (tc, spine) <- getTypeClass expr
  let nConstraint = WithContext normConstraint ctx
  progress <- solve tc nConstraint spine
  handleConstraintProgress (WithContext normConstraint ctx) progress

--------------------------------------------------------------------------------
-- Constraint solving

type MonadPolaritySolver m = TCM PolarityBuiltin m

type PolaritySolver =
  forall m.
  (MonadPolaritySolver m) =>
  WithContext PolarityTypeClassConstraint ->
  [PolarityNormType] ->
  m PolarityConstraintProgress

solve :: PolarityTypeClass -> PolaritySolver
solve = \case
  NegPolarity -> solveNegPolarity
  QuantifierPolarity q -> solveQuantifierPolarity q
  AddPolarity q -> solveAddPolarity q
  EqPolarity eq -> solveEqPolarity eq
  ImpliesPolarity -> solveImplPolarity
  MaxPolarity -> solveMaxPolarity
  FunctionPolarity position -> solveFunctionPolarity position
  IfPolarity -> solveIfCondPolarity

solveNegPolarity :: PolaritySolver
solveNegPolarity c [arg1, res] = case arg1 of
  (getNMeta -> Just m) -> blockOn [m]
  VPolarityExpr pol -> do
    let ctx = contextOf c
    let resPol = VPolarityExpr $ negatePolarity (provenanceOf ctx) pol
    resEq <- unify ctx res resPol
    return $ Progress [resEq]
  _ -> malformedConstraintError c
solveNegPolarity c _ = malformedConstraintError c

solveQuantifierPolarity :: Quantifier -> PolaritySolver
solveQuantifierPolarity q c [lam, res] = case lam of
  (getNMeta -> Just m) -> blockOn [m]
  (VPi binder resPol) -> do
    let ctx = contextOf c
    binderEq <- unify ctx (typeOf binder) (VPolarityExpr Unquantified)
    let tc = PolarityTypeClass $ AddPolarity q
    (_, addConstraint) <- createTC ctx (VBuiltin (CType tc) [resPol, res])
    return $ Progress [binderEq, addConstraint]
  _ -> malformedConstraintError c
solveQuantifierPolarity _ c _ = malformedConstraintError c

solveAddPolarity :: Quantifier -> PolaritySolver
solveAddPolarity q c [arg, res] = case arg of
  (getNMeta -> Just m) -> blockOn [m]
  VPolarityExpr inputPol -> do
    let ctx = contextOf c
    let p = originalProvenance ctx
    let resPol = VPolarityExpr $ addPolarity p q inputPol
    domEq <- unify ctx res resPol
    return $ Progress [domEq]
  _ -> malformedConstraintError c
solveAddPolarity _ c _ = malformedConstraintError c

solveMaxPolarity :: PolaritySolver
solveMaxPolarity c [arg1, arg2, res] = case (arg1, arg2) of
  (VPolarityExpr pol1, VPolarityExpr pol2) -> do
    let ctx = contextOf c
    let pol3 = VPolarityExpr $ maxPolarity pol1 pol2
    resEq <- unify ctx res pol3
    return $ Progress [resEq]
  (_, VPolarityExpr Unquantified) -> do
    resEq <- unify (contextOf c) arg1 res
    return $ Progress [resEq]
  (VPolarityExpr Unquantified, _) -> do
    resEq <- unify (contextOf c) arg2 res
    return $ Progress [resEq]
  (getNMeta -> Just m1, _) -> blockOn [m1]
  (_, getNMeta -> Just m2) -> blockOn [m2]
  _ -> malformedConstraintError c
solveMaxPolarity c _ = malformedConstraintError c

solveEqPolarity :: EqualityOp -> PolaritySolver
solveEqPolarity eq c [arg1, arg2, res] = case (arg1, arg2) of
  (VPolarityExpr pol1, VPolarityExpr pol2) -> do
    let ctx = contextOf c
    let pol3 = VPolarityExpr $ eqPolarity eq (provenanceOf ctx) pol1 pol2
    resEq <- unify ctx res pol3
    return $ Progress [resEq]
  (getNMeta -> Just m1, _) -> blockOn [m1]
  (_, getNMeta -> Just m2) -> blockOn [m2]
  _ -> malformedConstraintError c
solveEqPolarity _ c _ = malformedConstraintError c

solveImplPolarity :: PolaritySolver
solveImplPolarity c [arg1, arg2, res] = case (arg1, arg2) of
  (VPolarityExpr pol1, VPolarityExpr pol2) -> do
    let ctx = contextOf c
    let pol3 = VPolarityExpr $ implPolarity (provenanceOf ctx) pol1 pol2
    resEq <- unify ctx res pol3
    return $ Progress [resEq]
  (getNMeta -> Just m1, _) -> blockOn [m1]
  (_, getNMeta -> Just m2) -> blockOn [m2]
  _ -> malformedConstraintError c
solveImplPolarity c _ = malformedConstraintError c

solveFunctionPolarity :: FunctionPosition -> PolaritySolver
solveFunctionPolarity functionPosition c [arg, res] = case (arg, res) of
  (getNMeta -> Just m1, _) -> blockOn [m1]
  (VPolarityExpr pol, _) -> do
    let ctx = contextOf c
    let p = provenanceOf ctx
    let addFuncProv pp = PolFunctionProvenance p pp functionPosition
    let pol3 = VPolarityExpr $ mapPolarityProvenance addFuncProv pol
    resEq <- unify ctx res pol3
    return $ Progress [resEq]
  (VPi binder1 body1, VPi binder2 body2) -> do
    let ctx = contextOf c
    let tc = PolarityTypeClass $ FunctionPolarity functionPosition
    (_, binderConstraint) <- createTC ctx (VBuiltin (CType tc) [typeOf binder1, typeOf binder2])
    (_, bodyConstraint) <- createTC ctx (VBuiltin (CType tc) [body1, body2])
    return $ Progress [binderConstraint, bodyConstraint]
  _ -> malformedConstraintError c
solveFunctionPolarity _ c _ = malformedConstraintError c

solveIfCondPolarity :: PolaritySolver
solveIfCondPolarity c [pCond, pArg1, pArg2, pRes] = case pCond of
  (getNMeta -> Just m1) -> blockOn [m1]
  VPolarityExpr pol -> case pol of
    Unquantified -> solveMaxPolarity c [pArg1, pArg2, pRes]
    _ -> throwError $ QuantifiedIfCondition (contextOf c)
  _ -> malformedConstraintError c
solveIfCondPolarity c _ = malformedConstraintError c

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
-- Other

handleConstraintProgress ::
  (MonadTypeChecker PolarityBuiltin m) =>
  WithContext (TypeClassConstraint PolarityBuiltin) ->
  ConstraintProgress PolarityBuiltin ->
  m ()
handleConstraintProgress originalConstraint@(WithContext (Has m _) ctx) = \case
  Stuck metas -> do
    let blockedConstraint = blockConstraintOn (mapObject TypeClassConstraint originalConstraint) metas
    addConstraints [blockedConstraint]
  Progress newConstraints -> do
    solveMeta m (Builtin (provenanceOf ctx) (CConstructor LUnit)) (boundContext ctx)
    addConstraints newConstraints

getTypeClass :: (MonadCompile m) => PolarityNormExpr -> m (PolarityTypeClass, PolarityExplicitSpine)
getTypeClass = \case
  (VBuiltin (CType (PolarityTypeClass tc)) args) -> return (tc, args)
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."
