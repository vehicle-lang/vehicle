module Vehicle.Compile.Type.ConstraintSolver.Linearity where

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.ConstraintSolver.Core

--------------------------------------------------------------------------------
-- Operations over linearities

maxLinearity :: Linearity -> Linearity -> Linearity
maxLinearity l1 l2 = if l1 >= l2 then l1 else l2

mulLinearity :: Provenance -> Linearity -> Linearity -> Linearity
mulLinearity p l1 l2 = case (l1, l2) of
  (Constant, _)          -> l2
  (_, Constant)          -> l1
  (Linear p1, Linear p2) -> NonLinear p p1 p2
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

    (LinearityExpr _ l1, LinearityExpr _ l2) -> do
      let p = originalProvenance (constraintContext c)
      let linRes = LinearityExpr p $ mulLinearity p l1 l2
      return $ Progress [unify c res linRes]

    _ -> malformedConstraintError c

solveMulLinearity c _ = malformedConstraintError c

solveFunctionLinearity :: MonadMeta m
                            => FunctionPosition
                            -> Constraint
                            -> [CheckedExpr]
                            -> m ConstraintProgress
solveFunctionLinearity functionPosition c [arg, res] = case arg of
  (exprHead -> Meta _ m1) -> blockOn [m1]
  LinearityExpr _ lin     -> do
    let p = provenanceOf c
    let addFuncProv pp = LinFunctionProvenance p pp functionPosition
    let resLin = LinearityExpr p $ mapLinearityProvenance addFuncProv lin
    return $ Progress [unify c res resLin]
  _                       -> malformedConstraintError c

solveFunctionLinearity _ c _ = malformedConstraintError c