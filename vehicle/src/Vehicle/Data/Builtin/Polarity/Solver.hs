module Vehicle.Data.Builtin.Polarity.Solver
  ( solvePolarityConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface.Type
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Context.Generic

solvePolarityConstraint ::
  (MonadPolaritySolver m) =>
  WithContext (InstanceConstraint PolarityBuiltin) ->
  m ()
solvePolarityConstraint (WithContext normConstraint@(Resolve origin _ _ _ goal) ctx) = do
  (tc, spine) <- getTypeClass goal
  maybeProgress <- solve tc (ctx, origin) (mapMaybe getExplicitArg spine)
  let nConstraint = WithContext normConstraint ctx
  case maybeProgress of
    Nothing -> malformedConstraintError nConstraint
    Just progress -> do
      let solution = Forced $ VBuiltin (PolarityConstructor UnitLiteral) []
      handleAuxiliaryConstraintProgress solution nConstraint progress

--------------------------------------------------------------------------------
-- Constraint solving

pattern VPolarityExpr :: Polarity -> ForcedValueWithMetas PolarityBuiltin
pattern VPolarityExpr l <- VBuiltin (Polarity l) []
  where
    VPolarityExpr l = VBuiltin (Polarity l) []

type MonadPolaritySolver m =
  ( MonadTypeChecker PolarityBuiltin m,
    TypableBuiltin PolarityBuiltin
  )

type PolaritySolver =
  forall m.
  (MonadPolaritySolver m) =>
  InstanceConstraintInfo PolarityBuiltin ->
  [ThunkWithMetas PolarityBuiltin] ->
  m (Maybe (AuxiliaryConstraintProgress PolarityBuiltin))

solve :: PolarityRelation -> PolaritySolver
solve = \case
  NegPolarity -> solveNegPolarity
  QuantifierPolarity q -> solveQuantifierPolarity q
  AddPolarity p q -> solveAddPolarityOp p q
  ImpliesPolarity -> solveImplPolarity
  MaxPolarity -> solveMaxPolarityOp
  FunctionPolarity position -> solveFunctionPolarity position
  IfPolarity -> solveIfCondPolarity

solveNegPolarity :: PolaritySolver
solveNegPolarity info@(ctx, _) [arg1, res] = do
  (fArg1, blockingMetas) <- forceThunkWithMetas (namedBoundCtxOf ctx) arg1
  case fArg1 of
    VPolarityExpr pol -> do
      let resPol = Forced $ VPolarityExpr $ negatePolarity (provenanceOf ctx) pol
      resEq <- createInstanceUnification info res resPol
      return $ Just $ Progress [resEq] []
    _ -> blockOn blockingMetas
solveNegPolarity _ _ = return Nothing

solveQuantifierPolarity :: Quantifier -> PolaritySolver
solveQuantifierPolarity q info@(ctx, _) [lam, res] = do
  (fLam, blockingMetas) <- forceThunkWithMetas (namedBoundCtxOf ctx) lam
  case fLam of
    VPi binder resPol -> do
      let (_, p) = getNamedBinderInfo binder
      binderEq <- createInstanceUnification info (typeOf binder) (Forced $ VPolarityExpr Unquantified)
      let tc = PolarityRelation $ AddPolarity p q
      let resultPolarity = extendClosureWithBound resPol binder (boundCtxLv $ boundContext ctx)
      (_, addConstraint) <- createDerivedInstanceConstraint info Irrelevant (Forced $ VBuiltin tc (explicit <$> [resultPolarity, res]))
      return $ Just $ Progress [binderEq] [addConstraint]
    _ -> blockOn blockingMetas
solveQuantifierPolarity _ _c _ = return Nothing

solveAddPolarityOp :: Provenance -> Quantifier -> PolaritySolver
solveAddPolarityOp p q info@(ctx, _) [arg, res] = do
  (fArg, blockingMetas) <- forceThunkWithMetas (namedBoundCtxOf ctx) arg
  case fArg of
    VPolarityExpr inputPol -> do
      let resPol = Forced $ VPolarityExpr $ addPolarityOp p q inputPol
      domEq <- createInstanceUnification info res resPol
      return $ Just $ Progress [domEq] []
    _ -> blockOn blockingMetas
solveAddPolarityOp _ _ _ _ = return Nothing

solveMaxPolarityOp :: PolaritySolver
solveMaxPolarityOp info@(ctx, _) [arg1, arg2, res] = do
  (fArg1, blockingMetas1) <- forceThunkWithMetas (namedBoundCtxOf ctx) arg1
  (fArg2, blockingMetas2) <- forceThunkWithMetas (namedBoundCtxOf ctx) arg2
  case (fArg1, fArg2) of
    (VPolarityExpr pol1, VPolarityExpr pol2) -> do
      let pol3 = Forced $ VPolarityExpr $ maxPolarityOp pol1 pol2
      resEq <- createInstanceUnification info res pol3
      return $ Just $ Progress [resEq] []
    (_, VPolarityExpr Unquantified) -> do
      resEq <- createInstanceUnification info arg1 res
      return $ Just $ Progress [resEq] []
    (VPolarityExpr Unquantified, _) -> do
      resEq <- createInstanceUnification info arg2 res
      return $ Just $ Progress [resEq] []
    _ -> blockOn (blockingMetas1 <> blockingMetas2)
solveMaxPolarityOp _ _ = return Nothing

solveImplPolarity :: PolaritySolver
solveImplPolarity info@(ctx, _) [arg1, arg2, res] = do
  (fArg1, blockingMetas1) <- forceThunkWithMetas (namedBoundCtxOf ctx) arg1
  (fArg2, blockingMetas2) <- forceThunkWithMetas (namedBoundCtxOf ctx) arg2
  case (fArg1, fArg2) of
    (VPolarityExpr pol1, VPolarityExpr pol2) -> do
      let pol3 = Forced $ VPolarityExpr $ implPolarityOp (provenanceOf ctx) pol1 pol2
      resEq <- createInstanceUnification info res pol3
      return $ Just $ Progress [resEq] []
    _ -> blockOn (blockingMetas1 <> blockingMetas2)
solveImplPolarity _ _ = return Nothing

solveFunctionPolarity :: FunctionPosition -> PolaritySolver
solveFunctionPolarity functionPosition info@(ctx, _) [arg, res] = do
  (fArg, blockingMetas1) <- forceThunkWithMetas (namedBoundCtxOf ctx) arg
  (fRes, blockingMetas2) <- forceThunkWithMetas (namedBoundCtxOf ctx) res
  case (fArg, fRes) of
    (VPolarityExpr pol, _) -> do
      let p = provenanceOf ctx
      let addFuncProv pp = PolFunctionProvenance p pp functionPosition
      let pol3 = Forced $ VPolarityExpr $ mapPolarityProvenance addFuncProv pol
      resEq <- createInstanceUnification info res pol3
      return $ Just $ Progress [resEq] []
    (VPi binder1 closure1, VPi binder2 closure2) -> do
      let tc = PolarityRelation $ FunctionPolarity functionPosition
      (_, binderConstraint) <- createDerivedInstanceConstraint info Irrelevant (Forced $ VBuiltin tc (explicit <$> [typeOf binder1, typeOf binder2]))
      let lv = boundCtxLv $ toNamedBoundCtx $ boundContext ctx
      let body1 = extendClosureWithBound closure1 binder1 lv
      let body2 = extendClosureWithBound closure2 binder2 lv
      (_, bodyConstraint) <- createDerivedInstanceConstraint info Irrelevant (Forced $ VBuiltin tc (explicit <$> [body1, body2]))
      return $ Just $ Progress [] [binderConstraint, bodyConstraint]
    _ -> blockOn $ blockingMetas1 <> blockingMetas2
solveFunctionPolarity _ _ _ = return Nothing

solveIfCondPolarity :: PolaritySolver
solveIfCondPolarity info@(ctx, _) [pCond, pArg1, pArg2, pRes] = do
  (fCond, blockingMetas) <- forceThunkWithMetas (namedBoundCtxOf ctx) pCond
  case fCond of
    VPolarityExpr pol -> case pol of
      Unquantified -> solveMaxPolarityOp info [pArg1, pArg2, pRes]
      _ -> throwError $ QuantifiedIfCondition ctx
    _ -> blockOn blockingMetas
solveIfCondPolarity _ _ = return Nothing

--------------------------------------------------------------------------------
-- Operations over polarities

negPolarityOp ::
  (PolarityProvenance -> PolarityProvenance) ->
  Polarity ->
  Polarity
negPolarityOp modProv pol =
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
negatePolarity p = negPolarityOp (NegateProvenance p)

addPolarityOp :: Provenance -> Quantifier -> Polarity -> Polarity
addPolarityOp p q pol = case pol of
  Unquantified -> Quantified q (QuantifierProvenance p)
  Quantified q' pp -> if q == q' then pol else MixedSequential q p pp
  MixedParallel pp1 pp2 -> MixedSequential q p (if q == Forall then pp2 else pp1)
  MixedSequential {} -> pol

maxPolarityOp :: Polarity -> Polarity -> Polarity
maxPolarityOp pol1 pol2 = case (pol1, pol2) of
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

implPolarityOp ::
  Provenance ->
  Polarity ->
  Polarity ->
  Polarity
implPolarityOp p pol1 pol2 =
  let negPol = negPolarityOp (LHSImpliesProvenance p)
   in -- `a => b` = not a or b
      maxPolarityOp (negPol pol1) pol2

--------------------------------------------------------------------------------
-- Other

getTypeClass ::
  (MonadCompile m) =>
  InstanceGoal PolarityBuiltin ->
  m (PolarityRelation, UnforcedSpineWithMetas PolarityBuiltin)
getTypeClass = \case
  (InstanceGoal _ (Right (PolarityRelation tc)) args) -> return (tc, args)
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."
