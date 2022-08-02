module Vehicle.Compile.Type.ConstraintSolver.Linearity where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.ConstraintSolver.Core

--------------------------------------------------------------------------------
-- Operations over linearities

maxLinearity :: Linearity -> Linearity -> Linearity
maxLinearity l1 l2 = if l1 >= l2 then l1 else l2

mulLinearity :: Linearity -> Linearity -> Linearity
mulLinearity l1 l2 = case (l1, l2) of
  (Constant, _)          -> l2
  (_, Constant)          -> l1
  (Linear p1, Linear p2) -> NonLinear p1 p2
  (NonLinear{}, _)       -> l1
  (_, NonLinear{})       -> l2

--------------------------------------------------------------------------------
-- Constraint solving

solveMaxLinearity :: MonadMeta m
                  => Constraint
                  -> [CheckedExpr]
                  -> m ConstraintProgress
solveMaxLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (exprHead -> Meta _ m1, _) -> blockOn [m1]
    (_, exprHead -> Meta _ m2) -> blockOn [m2]

    (LinearityExpr p l1, LinearityExpr _ l2) -> do
      let linRes = LinearityExpr p $ maxLinearity l1 l2
      return $ Progress [unify c res linRes]

    _ -> malformedConstraintError c

solveMaxLinearity c _ = malformedConstraintError c


solveMulLinearity :: MonadMeta m
                  => Constraint
                  -> [CheckedExpr]
                  -> m ConstraintProgress
solveMulLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (exprHead -> Meta _ m1, _) -> blockOn [m1]
    (_, exprHead -> Meta _ m2) -> blockOn [m2]

    (LinearityExpr p l1, LinearityExpr _ l2) -> do
      let linRes = LinearityExpr p $ mulLinearity l1 l2
      return $ Progress [unify c res linRes]

    _ -> malformedConstraintError c

solveMulLinearity c _ = malformedConstraintError c
