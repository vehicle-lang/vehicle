{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|>" #-}
module Vehicle.Compile.Type.Force where

import Data.Maybe (fromMaybe, isJust)
import Vehicle.Compile.Context.Free
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap (lookup)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (singleton, unions)
import Vehicle.Data.BuiltinInterface (HasStandardData (getBuiltinFunction))
import Vehicle.Data.NormalisedExpr

-----------------------------------------------------------------------------
-- Meta-variable forcing

-- | Recursively forces the evaluation of any meta-variables at the head
-- of the expresson.
forceHead ::
  (MonadFreeContext builtin m) =>
  MetaSubstitution builtin ->
  ConstraintContext builtin ->
  WHNFValue builtin ->
  m (WHNFValue builtin, MetaSet)
forceHead subst ctx expr = do
  (maybeForcedExpr, blockingMetas) <- forceExpr subst expr
  forcedExpr <- case maybeForcedExpr of
    Nothing -> return expr
    Just forcedExpr -> do
      let dbCtx = boundContextOf ctx
      logDebug MaxDetail $ "forced" <+> prettyFriendly (WithContext expr dbCtx) <+> "to" <+> prettyFriendly (WithContext forcedExpr dbCtx)
      return forcedExpr
  return (forcedExpr, blockingMetas)

-- | Recursively forces the evaluation of any meta-variables that are blocking
-- evaluation.
forceExpr ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  MetaSubstitution builtin ->
  WHNFValue builtin ->
  m (Maybe (WHNFValue builtin), MetaSet)
forceExpr subst = go
  where
    go :: WHNFValue builtin -> m (Maybe (WHNFValue builtin), MetaSet)
    go = \case
      VMeta m spine -> goMeta m spine
      VBuiltin b spine -> forceBuiltin subst b spine
      _ -> return (Nothing, mempty)

    goMeta :: MetaID -> WHNFSpine builtin -> m (Maybe (WHNFValue builtin), MetaSet)
    goMeta m spine = do
      case MetaMap.lookup m subst of
        Just solution -> do
          normMetaExpr <- evalApp defaultNBEOptions (normalised solution) spine
          (maybeForcedExpr, blockingMetas) <- go normMetaExpr
          let forcedExpr = maybe (Just normMetaExpr) Just maybeForcedExpr
          return (forcedExpr, blockingMetas)
        Nothing -> return (Nothing, MetaSet.singleton m)

forceArg ::
  (MonadFreeContext builtin m) =>
  MetaSubstitution builtin ->
  WHNFArg builtin ->
  m (WHNFArg builtin, (Bool, MetaSet))
forceArg subst arg = do
  (maybeResult, blockingMetas) <- unpairArg <$> traverse (forceExpr subst) arg
  let result = fmap (fromMaybe (argExpr arg)) maybeResult
  let reduced = isJust $ argExpr maybeResult
  return (result, (reduced, blockingMetas))

forceBuiltin ::
  (MonadFreeContext builtin m) =>
  MetaSubstitution builtin ->
  builtin ->
  WHNFSpine builtin ->
  m (Maybe (WHNFValue builtin), MetaSet)
forceBuiltin subst b spine = case getBuiltinFunction b of
  Nothing -> return (Nothing, mempty)
  Just {} -> do
    (argResults, argData) <- unzip <$> traverse (forceArg subst) spine
    let (argsReduced, argBlockingMetas) = unzip argData
    let anyArgsReduced = or argsReduced
    let blockingMetas = MetaSet.unions argBlockingMetas
    result <-
      if not anyArgsReduced
        then return Nothing
        else do
          Just <$> evalBuiltin (evalApp defaultNBEOptions) b argResults
    return (result, blockingMetas)
