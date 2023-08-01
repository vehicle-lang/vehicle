{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use max" #-}
module Vehicle.Compile.Type.Subsystem.Linearity.LinearitySolver
  ( solveLinearityConstraint,
  )
where

import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Monad (MonadNorm)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta.Substitution (substMetas)
import Vehicle.Compile.Type.Monad (MonadTypeChecker)
import Vehicle.Compile.Type.Monad.Class (addConstraints, solveMeta)
import Vehicle.Compile.Type.Subsystem.Linearity.Core
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.Normalised
import Vehicle.Syntax.Builtin

solveLinearityConstraint ::
  (MonadLinearitySolver m) =>
  InstanceCandidateDatabase LinearityBuiltin ->
  WithContext (InstanceConstraint LinearityBuiltin) ->
  m ()
solveLinearityConstraint _ (WithContext constraint ctx) = do
  normConstraint@(Has _ _ expr) <- substMetas constraint
  (tc, spine) <- getTypeClass expr
  let nConstraint = WithContext normConstraint ctx
  progress <- solve tc nConstraint (mapMaybe getExplicitArg spine)
  handleConstraintProgress (WithContext normConstraint ctx) progress

--------------------------------------------------------------------------------
-- Constraint solving

type MonadLinearitySolver m =
  ( MonadTypeChecker LinearityBuiltin m,
    MonadNorm LinearityBuiltin m
  )

type LinearitySolver =
  forall m.
  (MonadLinearitySolver m) =>
  WithContext (InstanceConstraint LinearityBuiltin) ->
  [VType LinearityBuiltin] ->
  m (ConstraintProgress LinearityBuiltin)

solve :: LinearityRelation -> LinearitySolver
solve = \case
  MaxLinearity -> solveMaxLinearity
  MulLinearity -> solveMulLinearity
  FunctionLinearity position -> solveFunctionLinearity position
  QuantifierLinearity q -> solveQuantifierLinearity q

solveQuantifierLinearity :: Quantifier -> LinearitySolver
solveQuantifierLinearity _ _ [getNMeta -> Just m, _] = blockOn [m]
solveQuantifierLinearity _ c [VPi binder body, res] = do
  let ctx = contextOf c
  let varName = getBinderName binder
  let domainLin = VLinearityExpr (Linear (QuantifiedVariableProvenance (provenanceOf binder) varName))
  domEq <- unify ctx (typeOf binder) domainLin
  resEq <- unify ctx res body
  return $ Progress [domEq, resEq]
solveQuantifierLinearity _ c _ = malformedConstraintError c

solveMaxLinearity :: LinearitySolver
solveMaxLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (VLinearityExpr l1, VLinearityExpr l2) -> do
      let linRes = VLinearityExpr $ maxLinearityOp l1 l2
      resEq <- unify (contextOf c) res linRes
      return $ Progress [resEq]
    (_, VLinearityExpr Constant) -> do
      resEq <- unify (contextOf c) lin1 res
      return $ Progress [resEq]
    (VLinearityExpr Constant, _) -> do
      resEq <- unify (contextOf c) lin2 res
      return $ Progress [resEq]
    (getNMeta -> Just m1, _) -> blockOn [m1]
    (_, getNMeta -> Just m2) -> blockOn [m2]
    _ -> malformedConstraintError c
solveMaxLinearity c _ = malformedConstraintError c

solveMulLinearity :: LinearitySolver
solveMulLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (VLinearityExpr l1, VLinearityExpr l2) -> do
      let ctx = contextOf c
      let p = originalProvenance ctx
      let linRes = VLinearityExpr $ mulLinearityOp p l1 l2
      resEq <- unify ctx res linRes
      return $ Progress [resEq]
    (_, VLinearityExpr Constant) -> do
      resEq <- unify (contextOf c) lin1 res
      return $ Progress [resEq]
    (VLinearityExpr Constant, _) -> do
      resEq <- unify (contextOf c) lin2 res
      return $ Progress [resEq]
    (getNMeta -> Just m1, _) -> blockOn [m1]
    (_, getNMeta -> Just m2) -> blockOn [m2]
    _ -> malformedConstraintError c
solveMulLinearity c _ = malformedConstraintError c

solveFunctionLinearity :: FunctionPosition -> LinearitySolver
solveFunctionLinearity functionPosition c [arg, res] = case arg of
  (getNMeta -> Just m1) -> blockOn [m1]
  VLinearityExpr lin -> do
    let ctx = contextOf c
    let p = provenanceOf ctx
    let addFuncProv pp = LinFunctionProvenance p pp functionPosition
    let resLin = VLinearityExpr $ mapLinearityProvenance addFuncProv lin
    resEq <- unify ctx res resLin
    return $ Progress [resEq]
  _ -> malformedConstraintError c
solveFunctionLinearity _ c _ = malformedConstraintError c

--------------------------------------------------------------------------------
-- Operations over linearities

maxLinearityOp :: Linearity -> Linearity -> Linearity
maxLinearityOp l1 l2 = if l1 >= l2 then l1 else l2

mulLinearityOp :: Provenance -> Linearity -> Linearity -> Linearity
mulLinearityOp p l1 l2 = case (l1, l2) of
  (Constant, _) -> l2
  (_, Constant) -> l1
  (Linear p1, Linear p2) -> NonLinear p p1 p2
  (NonLinear {}, _) -> l1
  (_, NonLinear {}) -> l2

--------------------------------------------------------------------------------
-- Other

handleConstraintProgress ::
  (MonadTypeChecker LinearityBuiltin m) =>
  WithContext (InstanceConstraint LinearityBuiltin) ->
  ConstraintProgress LinearityBuiltin ->
  m ()
handleConstraintProgress originalConstraint@(WithContext (Has m _ _) ctx) = \case
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
