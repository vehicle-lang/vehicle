

module Vehicle.Core.Compile.Type.TypeClass
  ( solveTypeClassConstraints
  )
  where

import Control.Monad ( when )
import Prettyprinter ( (<+>), Pretty(pretty) )

import Vehicle.Prelude
import Vehicle.Core.AST
import Vehicle.Core.Compile.Type.Core
import Vehicle.Core.Compile.Type.Meta
import Vehicle.Core.Compile.Type.WHNF ( whnf )
import Vehicle.Core.Print.Core ()

--------------------------------------------------------------------------------
-- Solution

-- To solve / synthesise typeClassConstraints :
--  1. Normalise each one in the current metactxt
--  2. Insert lambdas for each Pi type
--  3. Match against (a) the list of built-in instances; then (b) against elements in the context
--  4. If the constraint is a Z3 one, then ask Z3 to solve it (after normalisation)

type MonadTypeClassResolution m =
  ( MonadLogger m
  , MonadMeta m
  )

-- | Tries to solve the provided list of constraints, returning the resulting
-- substitution for the solved constraints and a list of unsolved constraints if
-- any.
solveTypeClassConstraints :: MonadTypeClassResolution m => m ()
solveTypeClassConstraints = do
  logDebug "Beginning type-class resolution"
  constraints <- getTypeClassConstraints

  logDebug $ "constraints:" <+> pretty constraints
  unsolvedConstraints <- solveConstraints constraints
  logDebug $ "solution:" <+> pretty constraints

  setTypeClassConstraints unsolvedConstraints
  logDebug "Ending type-class resolution\n"

solveConstraints :: MonadTypeClassResolution m
                 => [TypeClassConstraint]
                 -> m [TypeClassConstraint]
solveConstraints [] = return []
solveConstraints (c : cs) = do
  unsolvedConstraints <- solveConstraints cs
  solved <- solveConstraint c
  return $ if solved then unsolvedConstraints else c : unsolvedConstraints

solveConstraint :: MonadTypeClassResolution m
                => TypeClassConstraint
                -> m Bool
solveConstraint (m `Has` e) = do
  validTypeClass <- isValidTypeClass e
  when validTypeClass $
    metaSolved (prov e) m (PrimDict e)
  return validTypeClass

isValidTypeClass :: MonadTypeClassResolution m => CheckedExpr -> m Bool
isValidTypeClass e = case decomposeApp e of
  (Builtin _ tc, args) -> do
    whnfArgs <- traverse extractAndNormaliseArg args
    return $ case (tc, whnfArgs) of
      (HasEq,          [t1, t2]) -> solveHasEq t1 t2
      (HasOrd,         [t1, t2]) -> solveHasOrd t1 t2
      (IsTruth,        [t])      -> solveIsTruth t
      (IsContainer,    [t1, t2]) -> solveIsContainer t1 t2
      (IsNatural,      [t])      -> solveIsNatural t
      (IsIntegral,     [t])      -> solveIsIntegral t
      (IsRational,     [t])      -> solveIsRational t
      (IsReal,         [t])      -> solveIsReal t
      (IsQuantifiable, [t1, t2]) -> solveIsQuantifiable t1 t2
      _                          -> False
  _                    -> return False

extractAndNormaliseArg :: MonadTypeClassResolution m => CheckedArg -> m CheckedExpr
extractAndNormaliseArg (Arg _ Explicit e) = whnf e
extractAndNormaliseArg _ = developerError "Not expecting type-classes with non-explicit arguments"

-- TODO insert Bool classes

solveHasEq :: CheckedExpr -> CheckedExpr -> Bool
solveHasEq (Builtin _ Bool)  (Builtin _ Prop) = True
solveHasEq (Builtin _ Prop)  (Builtin _ Prop) = True
solveHasEq (Builtin _ Nat)   (Builtin _ Prop) = True
solveHasEq (Builtin _ Int)   (Builtin _ Prop) = True
solveHasEq (Builtin _ Real)  (Builtin _ Prop) = True
solveHasEq _ _ = False

solveHasOrd :: CheckedExpr -> CheckedExpr -> Bool
solveHasOrd (Builtin _ Nat)   (Builtin _ Prop) = True
solveHasOrd (Builtin _ Int)   (Builtin _ Prop) = True
solveHasOrd (Builtin _ Real)  (Builtin _ Prop) = True
solveHasOrd _ _ = False

solveIsTruth :: CheckedExpr -> Bool
solveIsTruth (Builtin _ Bool) = True
solveIsTruth (Builtin _ Prop) = True
solveIsTruth _ = False

-- TODO
solveIsContainer :: CheckedExpr -> CheckedExpr -> Bool
solveIsContainer _ _ = False

solveIsNatural :: CheckedExpr -> Bool
solveIsNatural (Builtin _ Nat)  = True
solveIsNatural (Builtin _ Int)  = True
solveIsNatural (Builtin _ Real) = True
solveIsNatural _ = False

solveIsIntegral :: CheckedExpr -> Bool
solveIsIntegral (Builtin _ Int)  = True
solveIsIntegral (Builtin _ Real) = True
solveIsIntegral _ = False

solveIsRational :: CheckedExpr -> Bool
solveIsRational (Builtin _ Real) = True
solveIsRational _ = False

solveIsReal :: CheckedExpr -> Bool
solveIsReal (Builtin _ Real) = True
solveIsReal _ = False

-- TODO
solveIsQuantifiable :: CheckedExpr -> CheckedExpr -> Bool
solveIsQuantifiable _ _ = False