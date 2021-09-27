
module Vehicle.Core.Compile.Type.TypeClass
  ( solveTypeClassConstraints
  )
  where

import Data.Functor ((<&>))
import Control.Monad ( when )
import Prettyprinter ( (<+>), Pretty(pretty), squotes )

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
  setTypeClassConstraints []

  constraints' <- substMetas <$> getMetaSubstitution <*> pure constraints

  logDebug $ "constraints:" <+> pretty constraints'

  progress <- mconcat `fmap` traverse solveConstraint constraints'

  subst <- getMetaSubstitution
  logDebug $ "current-solution:" <+> pretty subst <+> "\n"

  return $ not (isStuck progress)

solveConstraint :: MonadTCResolution m
                => TypeClassConstraint
                -> m Progress
solveConstraint constraint@(m `Has` e) = do
  logDebug $ "trying" <+> pretty m <> "?" <+> "~" <+> pretty e
  incrCallDepth
  result <- findInstance m e
  case result of
    Solved _ -> metaSolved (prov e) m (PrimDict e)
    _        -> addTypeClassConstraint constraint
  decrCallDepth
  return result

-- Takes in the type-class and the list of arguments and returns the list of those
-- that cannot be inferred from the others.
getNonInferableArgs :: Builtin -> [CheckedExpr] -> [CheckedExpr]
getNonInferableArgs IsContainer [tCont, _tElem] = [tCont]
getNonInferableArgs _           args            = args

findInstance :: MonadTCResolution m => Meta -> CheckedExpr -> m Progress
findInstance m e = do
  eWHNF <- whnf e
  case decomposeApp eWHNF of
    (Builtin _ tc, args) -> do
      argsWHNF <- traverse extractAndNormaliseArg args
      let block = blockOnMetas tc argsWHNF
      case (tc, argsWHNF) of
          (IsContainer,    [t1, t2]) -> block $ solveIsContainer m (prov e) t1 t2
          (HasEq,          [t1, t2]) -> block $ return $ solveHasEq m t1 t2
          (HasOrd,         [t1, t2]) -> block $ return $ solveHasOrd m t1 t2
          (IsTruth,        [t])      -> block $ return $ solveIsTruth m t
          (IsNatural,      [t])      -> block $ return $ solveIsNatural m t
          (IsIntegral,     [t])      -> block $ return $ solveIsIntegral m t
          (IsRational,     [t])      -> block $ return $ solveIsRational m t
          (IsReal,         [t])      -> block $ return $ solveIsReal m t
          (IsQuantifiable, [t1, t2]) -> block $ return $ solveIsQuantifiable m t1 t2
          _                          -> developerError $ "Unknown type-class" <+> squotes (pretty tc) <+> "args" <+> pretty argsWHNF
    _ -> developerError $ "Unknown type-class" <+> squotes (pretty eWHNF)

extractAndNormaliseArg :: MonadTCResolution m => CheckedArg -> m CheckedExpr
extractAndNormaliseArg (Arg _ Explicit e) = whnf e
extractAndNormaliseArg _ = developerError "Not expecting type-classes with non-explicit arguments"

blockOnMetas :: MonadTCResolution m => Builtin -> [CheckedExpr] -> m Progress -> m Progress
blockOnMetas tc args action = do
  let metas = filter isMeta (getNonInferableArgs tc args)
  logDebug $ pretty (getNonInferableArgs tc args)
  logDebug $ pretty metas
  if null metas
    then action
    else do
      logDebug $ "Blocked on non-inferable metas:" <+> pretty metas
      return Stuck

isMeta :: CheckedExpr -> Bool
isMeta e = case decomposeApp e of
  (Meta _ _, _) -> True
  _             -> False

-- TODO insert Bool classes

solveHasEq :: Meta -> CheckedExpr -> CheckedExpr -> Progress
solveHasEq m (Builtin _ Bool)  (Builtin _ Prop) = solved m
solveHasEq m (Builtin _ Prop)  (Builtin _ Prop) = solved m
solveHasEq m (Builtin _ Nat)   (Builtin _ Prop) = solved m
solveHasEq m (Builtin _ Int)   (Builtin _ Prop) = solved m
solveHasEq m (Builtin _ Real)  (Builtin _ Prop) = solved m
solveHasEq _ _ _                                = Stuck

solveHasOrd :: Meta -> CheckedExpr -> CheckedExpr -> Progress
solveHasOrd m (Builtin _ Nat)   (Builtin _ Prop) = solved m
solveHasOrd m (Builtin _ Int)   (Builtin _ Prop) = solved m
solveHasOrd m (Builtin _ Real)  (Builtin _ Prop) = solved m
solveHasOrd _ _ _                                = Stuck

solveIsTruth :: Meta -> CheckedExpr -> Progress
solveIsTruth m (Builtin _ Bool) = solved m
solveIsTruth m (Builtin _ Prop) = solved m
solveIsTruth _ _                = Stuck

solveIsContainer :: MonadTCResolution m => Meta -> Provenance -> CheckedExpr -> CheckedExpr -> m Progress
solveIsContainer m p tCont tElem = do
  tContElem <- getContainerElem tCont
  case tContElem of
    Nothing -> return Stuck
    Just t  -> if t == tElem
      then return $ solved m
      else do
        addUnificationConstraint $ makeConstraint p [] tElem t
        return $ PartiallySolved mempty
  where
    getContainerElem :: MonadTCResolution m => CheckedExpr -> m (Maybe CheckedExpr)
    getContainerElem t = case decomposeApp t of
      (Builtin _ List,   [tElem'])    -> whnf (argExpr tElem') <&> Just
      (Builtin _ Tensor, [tElem', _]) -> whnf (argExpr tElem') <&> Just
      _                               -> return Nothing

solveIsNatural :: Meta -> CheckedExpr -> Progress
solveIsNatural m (Builtin _ Nat)  = solved m
solveIsNatural m (Builtin _ Int)  = solved m
solveIsNatural m (Builtin _ Real) = solved m
solveIsNatural _ _                = Stuck

solveIsIntegral :: Meta -> CheckedExpr -> Progress
solveIsIntegral m (Builtin _ Int)  = solved m
solveIsIntegral m (Builtin _ Real) = solved m
solveIsIntegral _ _                = Stuck

solveIsRational :: Meta -> CheckedExpr -> Progress
solveIsRational m (Builtin _ Real) = solved m
solveIsRational _ _                = Stuck

solveIsReal :: Meta -> CheckedExpr -> Progress
solveIsReal m (Builtin _ Real) = solved m
solveIsReal _ _                = Stuck

-- TODO
solveIsQuantifiable :: Meta -> CheckedExpr -> CheckedExpr -> Progress
solveIsQuantifiable _ _ _ = Stuck