{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use max" #-}
module Vehicle.Compile.Type.Subsystem.Standard.Constraint.LinearitySolver
  ( solveLinearityConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Constraint.Core
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalised (getNMeta)

solveLinearityConstraint ::
  MonadCompile m =>
  LinearityTypeClass ->
  WithContext StandardTypeClassConstraint ->
  [StandardNormType] ->
  m StandardConstraintProgress
solveLinearityConstraint = \case
  MaxLinearity -> solveMaxLinearity
  MulLinearity -> solveMulLinearity
  FunctionLinearity position -> solveFunctionLinearity position
  IfCondLinearity -> solveIfCondLinearity

--------------------------------------------------------------------------------
-- Operations over linearities

maxLinearity :: Linearity -> Linearity -> Linearity
maxLinearity l1 l2 = if l1 >= l2 then l1 else l2

mulLinearity :: Provenance -> Linearity -> Linearity -> Linearity
mulLinearity p l1 l2 = case (l1, l2) of
  (Constant, _) -> l2
  (_, Constant) -> l1
  (Linear p1, Linear p2) -> NonLinear p p1 p2
  (NonLinear {}, _) -> l1
  (_, NonLinear {}) -> l2

--------------------------------------------------------------------------------
-- Constraint solving

solveMaxLinearity ::
  MonadCompile m =>
  WithContext StandardTypeClassConstraint ->
  [StandardNormType] ->
  m StandardConstraintProgress
solveMaxLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (VLinearityExpr l1, VLinearityExpr l2) -> do
      let linRes = VLinearityExpr $ maxLinearity l1 l2
      return $ Progress [unify (contextOf c) res linRes]
    (_, VLinearityExpr Constant) ->
      return $ Progress [unify (contextOf c) lin1 res]
    (VLinearityExpr Constant, _) ->
      return $ Progress [unify (contextOf c) lin2 res]
    (getNMeta -> Just m1, _) -> blockOn [m1]
    (_, getNMeta -> Just m2) -> blockOn [m2]
    _ -> malformedConstraintError c
solveMaxLinearity c _ = malformedConstraintError c

solveMulLinearity ::
  MonadCompile m =>
  WithContext StandardTypeClassConstraint ->
  [StandardNormType] ->
  m StandardConstraintProgress
solveMulLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (VLinearityExpr l1, VLinearityExpr l2) -> do
      let ctx = contextOf c
      logDebug MaxDetail ("!!!!!" <+> pretty (provenanceOf ctx))
      let p = originalProvenance ctx
      logDebug MaxDetail ("!!!!!" <+> pretty (originalProvenance ctx))
      let linRes = VLinearityExpr $ mulLinearity p l1 l2
      return $ Progress [unify ctx res linRes]
    (_, VLinearityExpr Constant) ->
      return $ Progress [unify (contextOf c) lin1 res]
    (VLinearityExpr Constant, _) ->
      return $ Progress [unify (contextOf c) lin2 res]
    (getNMeta -> Just m1, _) -> blockOn [m1]
    (_, getNMeta -> Just m2) -> blockOn [m2]
    _ -> malformedConstraintError c
solveMulLinearity c _ = malformedConstraintError c

solveFunctionLinearity ::
  MonadCompile m =>
  FunctionPosition ->
  WithContext StandardTypeClassConstraint ->
  [StandardNormType] ->
  m StandardConstraintProgress
solveFunctionLinearity functionPosition c [arg, res] = case arg of
  (getNMeta -> Just m1) -> blockOn [m1]
  VLinearityExpr lin -> do
    let ctx = contextOf c
    let p = provenanceOf ctx
    let addFuncProv pp = LinFunctionProvenance p pp functionPosition
    let resLin = VLinearityExpr $ mapLinearityProvenance addFuncProv lin
    return $ Progress [unify ctx res resLin]
  _ -> malformedConstraintError c
solveFunctionLinearity _ c _ = malformedConstraintError c

solveIfCondLinearity ::
  MonadCompile m =>
  WithContext StandardTypeClassConstraint ->
  [StandardNormType] ->
  m StandardConstraintProgress
solveIfCondLinearity c [arg] = case arg of
  (getNMeta -> Just m1) -> blockOn [m1]
  VLinearityExpr lin -> case lin of
    Constant -> return $ Progress []
    Linear {} -> return $ Progress []
    NonLinear {} -> throwError $ NonLinearIfCondition (contextOf c)
  _ -> malformedConstraintError c
solveIfCondLinearity c _ = malformedConstraintError c
