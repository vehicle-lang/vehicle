module Vehicle.Data.Builtin.Linearity.Solver
  ( solveLinearityConstraint,
  )
where

import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad (MonadTypeChecker, forceThunkWithMetas)
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Interface.Type (TypableBuiltin)
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Variable.Bound.Context.Generic

solveLinearityConstraint ::
  (MonadLinearitySolver m) =>
  WithContext (InstanceConstraint LinearityBuiltin) ->
  m ()
solveLinearityConstraint (WithContext normConstraint@(Resolve origin _ _ _ goal) ctx) = do
  (tc, spine) <- getTypeClass goal
  let nConstraint = WithContext normConstraint ctx
  maybeProgress <- solve tc (ctx, origin) (mapMaybe getExplicitArg spine)
  case maybeProgress of
    Nothing -> malformedConstraintError nConstraint
    Just progress -> do
      let solution = Forced $ VBuiltin (LinearityConstructor UnitLiteral) []
      handleAuxiliaryConstraintProgress solution nConstraint progress

--------------------------------------------------------------------------------
-- Constraint solving

pattern VLinearityExpr :: Linearity -> ForcedValueWithMetas LinearityBuiltin
pattern VLinearityExpr l <- VBuiltin (Linearity l) []
  where
    VLinearityExpr l = VBuiltin (Linearity l) []

type MonadLinearitySolver m =
  ( MonadTypeChecker LinearityBuiltin m,
    TypableBuiltin LinearityBuiltin
  )

type LinearitySolver =
  forall m.
  (MonadLinearitySolver m) =>
  InstanceConstraintInfo LinearityBuiltin ->
  [ThunkWithMetas LinearityBuiltin] ->
  m (Maybe (AuxiliaryConstraintProgress LinearityBuiltin))

solve :: LinearityRelation -> LinearitySolver
solve = \case
  MaxLinearity -> solveOp2Linearity True True maxLinearityOp
  MulLinearity p -> solveOp2Linearity True True (mulLinearityOp p)
  DivLinearity p -> solveOp2Linearity False True (divLinearityOp p)
  PowLinearity p -> solveOp2Linearity False False (powLinearityOp p)
  FunctionLinearity position -> solveFunctionLinearity position
  QuantifierLinearity q -> solveQuantifierLinearity q

solveQuantifierLinearity :: Quantifier -> LinearitySolver
solveQuantifierLinearity _ info@(ctx, _) [fun, res] = do
  (fFun, blockingMetas) <- forceThunkWithMetas (namedBoundCtxOf ctx) fun
  case fFun of
    VPi binder closure -> do
      let (varName, p) = getNamedBinderInfo binder
      let domainLin = Forced $ VLinearityExpr (Linear (QuantifiedVariableProvenance p varName))
      domEq <- createInstanceUnification info (typeOf binder) domainLin
      let resultType = extendClosureWithBound closure binder (boundCtxLv $ boundContext ctx)
      resEq <- createInstanceUnification info res resultType
      return $ Just $ Progress [domEq, resEq] []
    _ -> blockOn blockingMetas
solveQuantifierLinearity _ _ _ = return Nothing

solveOp2Linearity ::
  Bool ->
  Bool ->
  (Linearity -> Linearity -> Linearity) ->
  LinearitySolver
solveOp2Linearity shortCircuitLHS shortCircuitRHS combine info@(ctx, _) [lin1, lin2, res] = do
  (fLin1, blockingMetas1) <- forceThunkWithMetas (namedBoundCtxOf ctx) lin1
  (fLin2, blockingMetas2) <- forceThunkWithMetas (namedBoundCtxOf ctx) lin2
  case (fLin1, fLin2) of
    (VLinearityExpr l1, VLinearityExpr l2) -> do
      let linRes = Forced $ VLinearityExpr $ combine l1 l2
      resEq <- createInstanceUnification info res linRes
      return $ Just $ Progress [resEq] []
    (VLinearityExpr Constant, _)
      | shortCircuitLHS -> do
          resEq <- createInstanceUnification info lin2 res
          return $ Just $ Progress [resEq] []
    (_, VLinearityExpr Constant)
      | shortCircuitRHS -> do
          resEq <- createInstanceUnification info lin1 res
          return $ Just $ Progress [resEq] []
    _ -> blockOn $ blockingMetas1 <> blockingMetas2
solveOp2Linearity _ _ _ _ _ = return Nothing

solveFunctionLinearity :: FunctionPosition -> LinearitySolver
solveFunctionLinearity functionPosition info@(ctx, _) [arg, res] = do
  (fArg, blockingMetas) <- forceThunkWithMetas (namedBoundCtxOf ctx) arg
  case fArg of
    VLinearityExpr lin -> do
      let p = provenanceOf ctx
      let addFuncProv pp = LinFunctionProvenance p pp functionPosition
      let resLin = Forced $ VLinearityExpr $ mapLinearityProvenance addFuncProv lin
      resEq <- createInstanceUnification info res resLin
      return $ Just $ Progress [resEq] []
    _ -> blockOn blockingMetas
solveFunctionLinearity _ _ _ = return Nothing

--------------------------------------------------------------------------------
-- Operations over linearities

maxLinearityOp :: Linearity -> Linearity -> Linearity
maxLinearityOp l1 l2 = case (l1, l2) of
  (Constant, _) -> l2
  (_, Constant) -> l1
  -- Note it's actually important that we return the left one here, as it ensures we print network output over network input.
  (Linear {}, Linear {}) -> l1
  (NonLinear {}, _) -> l1
  (_, NonLinear {}) -> l2

mulLinearityOp :: Provenance -> Linearity -> Linearity -> Linearity
mulLinearityOp p l1 l2 = case (l1, l2) of
  (Constant, _) -> l2
  (_, Constant) -> l1
  (Linear p1, Linear p2) -> NonLinear (LinearTimesLinear p p1 p2)
  (NonLinear {}, _) -> l1
  (_, NonLinear {}) -> l2

divLinearityOp :: Provenance -> Linearity -> Linearity -> Linearity
divLinearityOp p l1 l2 = case (l1, l2) of
  (_, Constant) -> l1
  (_, Linear p2) -> NonLinear (DivideByLinear p p2)
  (_, NonLinear {}) -> l2

powLinearityOp :: Provenance -> Linearity -> Linearity -> Linearity
powLinearityOp p l1 l2 = case (l1, l2) of
  (Constant, Constant) -> Constant
  (Linear p1, _) -> NonLinear (PowLinearBase p p1)
  (_, Linear p2) -> NonLinear (PowLinearExponent p p2)
  (NonLinear {}, _) -> l1
  (_, NonLinear {}) -> l2

--------------------------------------------------------------------------------
-- Other

getTypeClass ::
  (MonadCompile m) =>
  InstanceGoal LinearityBuiltin ->
  m (LinearityRelation, UnforcedSpineWithMetas LinearityBuiltin)
getTypeClass = \case
  (InstanceGoal [] (Right (LinearityRelation tc)) args) -> return (tc, args)
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."
