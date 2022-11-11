{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use max" #-}
module Vehicle.Compile.Type.ConstraintSolver.Linearity
  ( solveLinearityConstraint
  ) where

import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.ConstraintSolver.Core
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Monad

import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Normalise.NormExpr (pattern VLinearityExpr)

solveLinearityConstraint :: TCM m
                         => LinearityTypeClass
                         -> WithContext TypeClassConstraint
                         -> [CheckedType]
                         -> m ConstraintProgress
solveLinearityConstraint = \case
  MaxLinearity               -> solveMaxLinearity
  MulLinearity               -> solveMulLinearity
  FunctionLinearity position -> solveFunctionLinearity position
  IfCondLinearity            -> solveIfCondLinearity

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

solveMaxLinearity :: TCM m
                  => WithContext TypeClassConstraint
                  -> [CheckedExpr]
                  -> m ConstraintProgress
solveMaxLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (getMeta -> Just m1, _) -> blockOn [m1]
    (_, getMeta -> Just m2) -> blockOn [m2]

    (LinearityExpr p l1, LinearityExpr _ l2) -> do
      let linRes = VLinearityExpr p $ maxLinearity l1 l2
      nRes <- whnfNBE (length (boundContext (contextOf c))) res
      return $ Progress [unify (contextOf c) nRes linRes]

    _ -> malformedConstraintError c

solveMaxLinearity c _ = malformedConstraintError c


solveMulLinearity :: TCM m
                  => WithContext TypeClassConstraint
                  -> [CheckedExpr]
                  -> m ConstraintProgress
solveMulLinearity c [lin1, lin2, res] =
  case (lin1, lin2) of
    (getMeta -> Just m1, _) -> blockOn [m1]
    (_, getMeta -> Just m2) -> blockOn [m2]

    (LinearityExpr _ l1, LinearityExpr _ l2) -> do
      let ctx = contextOf c
      let p = originalProvenance ctx
      let linRes = VLinearityExpr p $ mulLinearity p l1 l2
      nRes <- whnfNBE (length (boundContext ctx)) res
      return $ Progress [unify ctx nRes linRes]

    _ -> malformedConstraintError c

solveMulLinearity c _ = malformedConstraintError c

solveFunctionLinearity :: TCM m
                       => FunctionPosition
                       -> WithContext TypeClassConstraint
                       -> [CheckedExpr]
                       -> m ConstraintProgress
solveFunctionLinearity functionPosition c [arg, res] = case arg of
  (getMeta -> Just m1) -> blockOn [m1]
  LinearityExpr _ lin     -> do
    let ctx = contextOf c
    let p = provenanceOf ctx
    let addFuncProv pp = LinFunctionProvenance p pp functionPosition
    let resLin = VLinearityExpr p $ mapLinearityProvenance addFuncProv lin
    nRes <- whnfNBE (length (boundContext ctx)) res
    return $ Progress [unify ctx nRes resLin]
  _                       -> malformedConstraintError c

solveFunctionLinearity _ c _ = malformedConstraintError c

solveIfCondLinearity :: TCM m
                     => WithContext TypeClassConstraint
                     -> [CheckedExpr]
                     -> m ConstraintProgress
solveIfCondLinearity c [arg] = case arg of
  (getMeta -> Just m1) -> blockOn [m1]
  LinearityExpr _ lin     -> case lin of
    Constant    -> return $ Progress []
    Linear{}    -> return $ Progress []
    NonLinear{} -> throwError $ NonLinearIfCondition (contextOf c)

  _ -> malformedConstraintError c

solveIfCondLinearity c _ = malformedConstraintError c
