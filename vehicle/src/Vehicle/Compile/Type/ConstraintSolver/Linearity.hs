{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use max" #-}
module Vehicle.Compile.Type.ConstraintSolver.Linearity
  ( solveLinearityConstraint,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.ConstraintSolver.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.Normalised (NormType, getMeta, pattern VLinearityExpr)

solveLinearityConstraint ::
  TCM m =>
  LinearityTypeClass ->
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
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
  TCM m =>
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveMaxLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (getMeta -> Just m1, _) -> blockOn [m1]
    (_, getMeta -> Just m2) -> blockOn [m2]
    (VLinearityExpr p l1, VLinearityExpr _ l2) -> do
      let linRes = VLinearityExpr p $ maxLinearity l1 l2
      return $ Progress [unify (contextOf c) res linRes]
    _ -> malformedConstraintError c
solveMaxLinearity c _ = malformedConstraintError c

solveMulLinearity ::
  TCM m =>
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveMulLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (getMeta -> Just m1, _) -> blockOn [m1]
    (_, getMeta -> Just m2) -> blockOn [m2]
    (VLinearityExpr _ l1, VLinearityExpr _ l2) -> do
      let ctx = contextOf c
      let p = originalProvenance ctx
      let linRes = VLinearityExpr p $ mulLinearity p l1 l2
      return $ Progress [unify ctx res linRes]
    _ -> malformedConstraintError c
solveMulLinearity c _ = malformedConstraintError c

solveFunctionLinearity ::
  TCM m =>
  FunctionPosition ->
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveFunctionLinearity functionPosition c [arg, res] = case arg of
  (getMeta -> Just m1) -> blockOn [m1]
  VLinearityExpr _ lin -> do
    let ctx = contextOf c
    let p = provenanceOf ctx
    let addFuncProv pp = LinFunctionProvenance p pp functionPosition
    let resLin = VLinearityExpr p $ mapLinearityProvenance addFuncProv lin
    return $ Progress [unify ctx res resLin]
  _ -> malformedConstraintError c
solveFunctionLinearity _ c _ = malformedConstraintError c

solveIfCondLinearity ::
  TCM m =>
  WithContext TypeClassConstraint ->
  [NormType] ->
  m ConstraintProgress
solveIfCondLinearity c [arg] = case arg of
  (getMeta -> Just m1) -> blockOn [m1]
  VLinearityExpr _ lin -> case lin of
    Constant -> return $ Progress []
    Linear {} -> return $ Progress []
    NonLinear {} -> throwError $ NonLinearIfCondition (contextOf c)
  _ -> malformedConstraintError c
solveIfCondLinearity c _ = malformedConstraintError c
