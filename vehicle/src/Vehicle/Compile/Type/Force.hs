{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|>" #-}
module Vehicle.Compile.Type.Force where

import Data.Data (Proxy (..))
import Data.Maybe (fromMaybe)
import Vehicle.Compile.Normalise.Builtin (NormalisableBuiltin (..))
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap (lookup)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (singleton, unions)
import Vehicle.Compile.Type.Monad (MonadTypeChecker)
import Vehicle.Compile.Type.Monad.Class (getMetaSubstitution)
import Vehicle.Data.Code.Value

-----------------------------------------------------------------------------
-- Meta-variable forcing

type MonadForce builtin m =
  ( MonadTypeChecker builtin m,
    NormalisableBuiltin builtin
  )

-- | Recursively forces the evaluation of any meta-variables at the head
-- of the expresson.
forceHead ::
  forall builtin m.
  (MonadForce builtin m) =>
  NamedBoundCtx ->
  Value builtin ->
  m (Value builtin, MetaSet)
forceHead ctx expr = do
  (maybeForcedExpr, blockingMetas) <- forceExpr expr
  forcedExpr <- case maybeForcedExpr of
    Nothing -> return expr
    Just forcedExpr -> do
      logDebug MaxDetail $
        "forced"
          <+> prettyFriendly (WithContext expr ctx)
          <+> "to"
          <+> prettyFriendly (WithContext forcedExpr ctx)
      return forcedExpr
  return (forcedExpr, blockingMetas)

-- | Recursively forces the evaluation of any meta-variables that are blocking
-- evaluation.
forceExpr ::
  (MonadForce builtin m) =>
  Value builtin ->
  m (Maybe (Value builtin), MetaSet)
forceExpr = \case
  VMeta m spine -> forceMeta m spine
  VBuiltin b spine -> forceBuiltin b spine
  _ -> return (Nothing, mempty)

forceMeta ::
  forall builtin m.
  (MonadForce builtin m) =>
  MetaID ->
  Spine builtin ->
  m (Maybe (Value builtin), MetaSet)
forceMeta m spine = do
  subst <- getMetaSubstitution (Proxy @builtin)
  case MetaMap.lookup m subst of
    Just solution -> do
      normMetaExpr <- normaliseApp (normalised solution) spine
      (maybeForcedExpr, blockingMetas) <- forceExpr normMetaExpr
      let forcedExpr = maybe (Just normMetaExpr) Just maybeForcedExpr
      return (forcedExpr, blockingMetas)
    Nothing -> return (Nothing, MetaSet.singleton m)

forceArg ::
  (MonadForce builtin m) =>
  VArg builtin ->
  m (Maybe (VArg builtin), MetaSet)
forceArg arg = do
  (maybeResult, blockingMetas) <- unpairArg <$> traverse forceExpr arg
  return (sequenceA maybeResult, blockingMetas)

forceBuiltin ::
  (MonadForce builtin m) =>
  builtin ->
  Spine builtin ->
  m (Maybe (Value builtin), MetaSet)
forceBuiltin b spine = do
  (maybeUnblockedSpine, blockingMetas) <- forceBuiltinSpine spine 0 (blockingArgs b)
  finalValue <- traverse (normaliseBuiltin b) maybeUnblockedSpine
  return (finalValue, blockingMetas)

forceBuiltinSpine ::
  (MonadForce builtin m) =>
  Spine builtin ->
  Int ->
  [Int] ->
  m (Maybe (Spine builtin), MetaSet)
forceBuiltinSpine [] _currentIndex _blockingArgs = return (Nothing, mempty)
forceBuiltinSpine _args _currentIndex [] = return (Nothing, mempty)
forceBuiltinSpine (arg : args) currentIndex (blockingIndex : blockingIndices) = do
  (maybeUnblockedArgs, argsBlockingMetas) <- forceBuiltinSpine args (currentIndex + 1) (blockingIndex : blockingIndices)

  if currentIndex /= blockingIndex
    then return ((arg :) <$> maybeUnblockedArgs, argsBlockingMetas)
    else do
      (maybeUnblockedArg, argBlockingMetas) <- forceArg arg
      let newBlockingMetas = MetaSet.unions [argBlockingMetas, argsBlockingMetas]
      let newFinalArgs = case maybeUnblockedArg of
            Just unblockedArg -> Just (unblockedArg : fromMaybe args maybeUnblockedArgs)
            Nothing -> (arg :) <$> maybeUnblockedArgs
      return (newFinalArgs, newBlockingMetas)
