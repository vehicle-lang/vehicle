{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|>" #-}
module Vehicle.Compile.Type.Force where

import Data.Maybe (fromMaybe)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Normalise.Builtin (NormalisableBuiltin (..))
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap (lookup)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (singleton, unions)
import Vehicle.Data.Code.Value

-----------------------------------------------------------------------------
-- Meta-variable forcing

type MonadForce builtin m =
  ( MonadFreeContext builtin m,
    NormalisableBuiltin builtin
  )

-- | Recursively forces the evaluation of any meta-variables at the head
-- of the expresson.
forceHead ::
  (MonadForce builtin m) =>
  MetaSubstitution builtin ->
  ConstraintContext builtin ->
  WHNFValue builtin ->
  m (WHNFValue builtin, MetaSet)
forceHead subst ctx expr = do
  (maybeForcedExpr, blockingMetas) <- forceExpr subst expr
  forcedExpr <- case maybeForcedExpr of
    Nothing -> return expr
    Just forcedExpr -> do
      let dbCtx = namedBoundCtxOf ctx
      logDebug MaxDetail $
        "forced"
          <+> prettyFriendly (WithContext expr dbCtx)
          <+> "to"
          <+> prettyFriendly (WithContext forcedExpr dbCtx)
      return forcedExpr
  return (forcedExpr, blockingMetas)

-- | Recursively forces the evaluation of any meta-variables that are blocking
-- evaluation.
forceExpr ::
  (MonadForce builtin m) =>
  MetaSubstitution builtin ->
  WHNFValue builtin ->
  m (Maybe (WHNFValue builtin), MetaSet)
forceExpr subst = \case
  VMeta m spine -> forceMeta subst m spine
  VBuiltin b spine -> forceBuiltin subst b spine
  _ -> return (Nothing, mempty)

forceMeta ::
  (MonadForce builtin m) =>
  MetaSubstitution builtin ->
  MetaID ->
  WHNFSpine builtin ->
  m (Maybe (WHNFValue builtin), MetaSet)
forceMeta subst m spine = do
  case MetaMap.lookup m subst of
    Just solution -> do
      normMetaExpr <- normaliseApp (normalised solution) spine
      (maybeForcedExpr, blockingMetas) <- forceExpr subst normMetaExpr
      let forcedExpr = maybe (Just normMetaExpr) Just maybeForcedExpr
      return (forcedExpr, blockingMetas)
    Nothing -> return (Nothing, MetaSet.singleton m)

forceArg ::
  (MonadForce builtin m) =>
  MetaSubstitution builtin ->
  WHNFArg builtin ->
  m (Maybe (WHNFArg builtin), MetaSet)
forceArg subst arg = do
  (maybeResult, blockingMetas) <- unpairArg <$> traverse (forceExpr subst) arg
  return (sequenceA maybeResult, blockingMetas)

forceBuiltin ::
  (MonadForce builtin m) =>
  MetaSubstitution builtin ->
  builtin ->
  WHNFSpine builtin ->
  m (Maybe (WHNFValue builtin), MetaSet)
forceBuiltin subst b spine = do
  logDebug MaxDetail $ prettyVerbose spine
  (maybeUnblockedSpine, blockingMetas) <- forceBuiltinSpine subst spine 0 (blockingArgs b)
  finalValue <- traverse (normaliseBuiltin b) maybeUnblockedSpine
  return (finalValue, blockingMetas)

forceBuiltinSpine ::
  (MonadForce builtin m) =>
  MetaSubstitution builtin ->
  WHNFSpine builtin ->
  Int ->
  [Int] ->
  m (Maybe (WHNFSpine builtin), MetaSet)
forceBuiltinSpine _subst [] _currentIndex _blockingArgs = return (Nothing, mempty)
forceBuiltinSpine _subst _args _currentIndex [] = return (Nothing, mempty)
forceBuiltinSpine subst (arg : args) currentIndex (blockingIndex : blockingIndices) = do
  (maybeUnblockedArgs, argsBlockingMetas) <- forceBuiltinSpine subst args (currentIndex + 1) (blockingIndex : blockingIndices)
  logDebug MaxDetail $ pretty currentIndex <+> pretty blockingIndex <+> prettyVerbose arg

  if currentIndex /= blockingIndex
    then return ((arg :) <$> maybeUnblockedArgs, argsBlockingMetas)
    else do
      (maybeUnblockedArg, argBlockingMetas) <- forceArg subst arg
      let newBlockingMetas = MetaSet.unions [argBlockingMetas, argsBlockingMetas]
      let newFinalArgs = case maybeUnblockedArg of
            Just unblockedArg -> Just (unblockedArg : fromMaybe args maybeUnblockedArgs)
            Nothing -> (arg :) <$> maybeUnblockedArgs
      return (newFinalArgs, newBlockingMetas)
