

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

type MonadTCResolution m =
  ( MonadLogger m
  , MonadMeta m
  )

-- | Tries to solve the provided list of constraints, returning
-- whether or not any progress was made.
solveTypeClassConstraints :: MonadTCResolution m => m Bool
solveTypeClassConstraints = do
  logDebug "Starting new type-class resolution pass"
  constraints <- getTypeClassConstraints
  constraints' <- substMetas <$> getMetaSubstitution <*> pure constraints

  logDebug $ "constraints:" <+> pretty constraints'
  unsolvedConstraints <- solveConstraints constraints'
  --logDebug $ "solution:" <+> pretty constraints'

  setTypeClassConstraints unsolvedConstraints

  subst <- getMetaSubstitution
  logDebug $ "current-solution:" <+> pretty subst <+> "\n"

  return $ length unsolvedConstraints < length constraints

solveConstraints :: MonadTCResolution m
                 => [TypeClassConstraint]
                 -> m [TypeClassConstraint]
solveConstraints [] = return []
solveConstraints (c : cs) = do
  unsolvedConstraints <- solveConstraints cs
  madeProgress <- solveConstraint c
  return $ if madeProgress
    then unsolvedConstraints
    else c : unsolvedConstraints

solveConstraint :: MonadTCResolution m
                => TypeClassConstraint
                -> m Bool
solveConstraint (m `Has` e) = do
  validTypeClass <- isValidTypeClass e
  when validTypeClass $
    metaSolved (prov e) m (PrimDict e)
  return validTypeClass

isValidTypeClass :: MonadTCResolution m => CheckedExpr -> m Bool
isValidTypeClass e = case decomposeApp e of
  (Builtin _ tc, args) -> do
    whnfArgs <- traverse extractAndNormaliseArg args
    case (tc, whnfArgs) of
      (IsContainer,    [t1, t2]) -> solveIsContainer (prov e) t1 t2
      (HasEq,          [t1, t2]) -> return $ solveHasEq t1 t2
      (HasOrd,         [t1, t2]) -> return $ solveHasOrd t1 t2
      (IsTruth,        [t])      -> return $ solveIsTruth t
      (IsNatural,      [t])      -> return $ solveIsNatural t
      (IsIntegral,     [t])      -> return $ solveIsIntegral t
      (IsRational,     [t])      -> return $ solveIsRational t
      (IsReal,         [t])      -> return $ solveIsReal t
      (IsQuantifiable, [t1, t2]) -> return $ solveIsQuantifiable t1 t2
      _                          -> return False
  _                    -> return False

extractAndNormaliseArg :: MonadTCResolution m => CheckedArg -> m CheckedExpr
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

solveIsContainer :: MonadTCResolution m => Provenance -> CheckedExpr -> CheckedExpr -> m Bool
solveIsContainer p tCont tElem = case tContElem of
  Nothing -> return False
  Just t  -> if t == tElem
    then return True
    else do
      addUnificationConstraint $ makeConstraint p [] tElem t
      return True
  where
    tContElem :: Maybe CheckedExpr
    tContElem = case decomposeApp tCont of
      (Builtin _ List,   [tElem'])    -> Just $ argExpr tElem'
      (Builtin _ Tensor, [tElem', _]) -> Just $ argExpr tElem'
      _ -> Nothing

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