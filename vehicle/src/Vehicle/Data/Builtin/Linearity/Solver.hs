module Vehicle.Data.Builtin.Linearity.Solver
  ( solveLinearityConstraint,
  )
where

import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.NBE (normaliseClosure)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad (MonadTypeChecker)
import Vehicle.Compile.Type.Monad.Class (substMetaVariables)
import Vehicle.Compile.Type.System
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Builtin.Linearity
import Vehicle.Data.Code.Value

solveLinearityConstraint ::
  (MonadLinearitySolver m) =>
  WithContext (InstanceConstraint LinearityBuiltin) ->
  m ()
solveLinearityConstraint constraintWithCtx = do
  substConstraintWithCtx@(WithContext normConstraint@(Resolve origin _ _ goal) ctx) <- substMetaVariables @LinearityBuiltin constraintWithCtx
  logDebug MaxDetail $ "Forced:" <+> prettyFriendly substConstraintWithCtx

  (tc, spine) <- getTypeClass goal
  let nConstraint = WithContext normConstraint ctx
  let maybeProgress = solve tc (ctx, origin) (mapMaybe getExplicitArg spine)
  case maybeProgress of
    Nothing -> malformedConstraintError nConstraint
    Just progress -> do
      let solution = VBuiltin (LinearityConstructor UnitLiteral) []
      handleAuxiliaryConstraintProgress solution nConstraint =<< progress

--------------------------------------------------------------------------------
-- Constraint solving

pattern VLinearityExpr :: Linearity -> Value LinearityBuiltin
pattern VLinearityExpr l <- VBuiltin (Linearity l) []
  where
    VLinearityExpr l = VBuiltin (Linearity l) []

type MonadLinearitySolver m =
  ( MonadTypeChecker LinearityBuiltin m
  )

type LinearitySolver =
  forall m.
  (MonadLinearitySolver m) =>
  InstanceConstraintInfo LinearityBuiltin ->
  [VType LinearityBuiltin] ->
  Maybe (m (AuxiliaryConstraintProgress LinearityBuiltin))

solve :: LinearityRelation -> LinearitySolver
solve = \case
  MaxLinearity -> solveOp2Linearity True True maxLinearityOp
  MulLinearity p -> solveOp2Linearity True True (mulLinearityOp p)
  DivLinearity p -> solveOp2Linearity False True (divLinearityOp p)
  PowLinearity p -> solveOp2Linearity False False (powLinearityOp p)
  FunctionLinearity position -> solveFunctionLinearity position
  QuantifierLinearity q -> solveQuantifierLinearity q

solveQuantifierLinearity :: Quantifier -> LinearitySolver
solveQuantifierLinearity _ _ [getNMeta -> Just m, _] = blockOn [m]
solveQuantifierLinearity _ info@(ctx, _) [VPi binder closure, res] = Just $ do
  let varName = getBinderName binder
  let domainLin = VLinearityExpr (Linear (QuantifiedVariableProvenance (provenanceOf binder) varName))
  domEq <- createInstanceUnification info (typeOf binder) domainLin
  resultType <- normaliseClosure (toNamedBoundCtx $ boundContext ctx) binder closure
  resEq <- createInstanceUnification info res resultType
  return $ Progress [domEq, resEq] []
solveQuantifierLinearity _ _ _ = Nothing

solveOp2Linearity ::
  Bool ->
  Bool ->
  (Linearity -> Linearity -> Linearity) ->
  LinearitySolver
solveOp2Linearity shortCircuitLHS shortCircuitRHS combine info [lin1, lin2, res] =
  case (lin1, lin2) of
    (VLinearityExpr l1, VLinearityExpr l2) -> Just $ do
      let linRes = VLinearityExpr $ combine l1 l2
      resEq <- createInstanceUnification info res linRes
      return $ Progress [resEq] []
    (VLinearityExpr Constant, _)
      | shortCircuitLHS -> Just $ do
          resEq <- createInstanceUnification info lin2 res
          return $ Progress [resEq] []
    (_, VLinearityExpr Constant)
      | shortCircuitRHS -> Just $ do
          resEq <- createInstanceUnification info lin1 res
          return $ Progress [resEq] []
    (getNMeta -> Just m1, _) -> blockOn [m1]
    (_, getNMeta -> Just m2) -> blockOn [m2]
    _ -> Nothing
solveOp2Linearity _ _ _ _ _ = Nothing

solveFunctionLinearity :: FunctionPosition -> LinearitySolver
solveFunctionLinearity functionPosition info@(ctx, _) [arg, res] = case arg of
  (getNMeta -> Just m1) -> blockOn [m1]
  VLinearityExpr lin -> Just $ do
    let p = provenanceOf ctx
    let addFuncProv pp = LinFunctionProvenance p pp functionPosition
    let resLin = VLinearityExpr $ mapLinearityProvenance addFuncProv lin
    resEq <- createInstanceUnification info res resLin
    return $ Progress [resEq] []
  _ -> Nothing
solveFunctionLinearity _ _ _ = Nothing

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

getTypeClass :: (MonadCompile m) => InstanceGoal LinearityBuiltin -> m (LinearityRelation, Spine LinearityBuiltin)
getTypeClass = \case
  (InstanceGoal [] (LinearityRelation tc) args) -> return (tc, args)
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."
