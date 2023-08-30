{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use max" #-}
module Vehicle.Backend.Queries.Error.Linearity.LinearitySolver
  ( solveLinearityConstraint,
  )
where

import Data.Maybe (mapMaybe)
import Vehicle.Backend.Queries.Error.Linearity.Core
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad (MonadTypeChecker)
import Vehicle.Compile.Type.Monad.Class (addConstraints, solveMeta, substMetas)
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.Builtin

solveLinearityConstraint ::
  (MonadLinearitySolver m) =>
  InstanceCandidateDatabase LinearityBuiltin ->
  WithContext (InstanceConstraint LinearityBuiltin) ->
  m ()
solveLinearityConstraint _ (WithContext constraint ctx) = do
  normConstraint@(Resolve origin _ _ expr) <- substMetas constraint
  logDebug MaxDetail $ "Forced:" <+> prettyFriendly (WithContext normConstraint ctx)

  (tc, spine) <- getTypeClass expr
  let nConstraint = WithContext normConstraint ctx
  let maybeProgress = solve tc (ctx, origin) (mapMaybe getExplicitArg spine)
  case maybeProgress of
    Nothing -> malformedConstraintError nConstraint
    Just progress -> handleConstraintProgress nConstraint =<< progress

--------------------------------------------------------------------------------
-- Constraint solving

type MonadLinearitySolver m =
  ( MonadTypeChecker LinearityBuiltin m
  )

type LinearitySolver =
  forall m.
  (MonadLinearitySolver m) =>
  InstanceConstraintInfo LinearityBuiltin ->
  [VType LinearityBuiltin] ->
  Maybe (m (ConstraintProgress LinearityBuiltin))

solve :: LinearityRelation -> LinearitySolver
solve = \case
  MaxLinearity -> solveOp2Linearity True True maxLinearityOp
  MulLinearity -> solveOp2Linearity True True mulLinearityOp
  DivLinearity -> solveOp2Linearity False True divLinearityOp
  PowLinearity -> solveOp2Linearity False False powLinearityOp
  FunctionLinearity position -> solveFunctionLinearity position
  QuantifierLinearity q -> solveQuantifierLinearity q

solveQuantifierLinearity :: Quantifier -> LinearitySolver
solveQuantifierLinearity _ _ [getNMeta -> Just m, _] = blockOn [m]
solveQuantifierLinearity _ info [VPi binder body, res] = Just $ do
  let varName = getBinderName binder
  let domainLin = VLinearityExpr (Linear (QuantifiedVariableProvenance (provenanceOf binder) varName))
  domEq <- createInstanceUnification info (typeOf binder) domainLin
  resEq <- createInstanceUnification info res body
  return $ Progress [domEq, resEq]
solveQuantifierLinearity _ _ _ = Nothing

solveOp2Linearity ::
  Bool ->
  Bool ->
  (Provenance -> Linearity -> Linearity -> Linearity) ->
  LinearitySolver
solveOp2Linearity shortCircuitLHS shortCircuitRHS combine info@(ctx, _) [lin1, lin2, res] =
  case (lin1, lin2) of
    (VLinearityExpr l1, VLinearityExpr l2) -> Just $ do
      let p = originalProvenance ctx
      let linRes = VLinearityExpr $ combine p l1 l2
      resEq <- createInstanceUnification info res linRes
      return $ Progress [resEq]
    (VLinearityExpr Constant, _)
      | shortCircuitLHS -> Just $ do
          resEq <- createInstanceUnification info lin2 res
          return $ Progress [resEq]
    (_, VLinearityExpr Constant)
      | shortCircuitRHS -> Just $ do
          resEq <- createInstanceUnification info lin1 res
          return $ Progress [resEq]
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
    return $ Progress [resEq]
  _ -> Nothing
solveFunctionLinearity _ _ _ = Nothing

--------------------------------------------------------------------------------
-- Operations over linearities

maxLinearityOp :: Provenance -> Linearity -> Linearity -> Linearity
maxLinearityOp _p l1 l2 = if l1 >= l2 then l1 else l2

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

handleConstraintProgress ::
  (MonadTypeChecker LinearityBuiltin m) =>
  WithContext (InstanceConstraint LinearityBuiltin) ->
  ConstraintProgress LinearityBuiltin ->
  m ()
handleConstraintProgress originalConstraint@(WithContext (Resolve _ m _ _) ctx) = \case
  Stuck metas -> do
    let blockedConstraint = blockConstraintOn (mapObject InstanceConstraint originalConstraint) metas
    addConstraints [blockedConstraint]
  Progress newConstraints -> do
    solveMeta m (UnitLiteral (provenanceOf ctx)) (boundContext ctx)
    addConstraints newConstraints

getTypeClass :: (MonadCompile m) => Value LinearityBuiltin -> m (LinearityRelation, Spine LinearityBuiltin)
getTypeClass = \case
  (VBuiltin (LinearityRelation tc) args) -> return (tc, args)
  _ -> compilerDeveloperError "Unexpected non-type-class instance argument found."
