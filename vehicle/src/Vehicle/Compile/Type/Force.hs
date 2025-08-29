{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <|>" #-}
module Vehicle.Compile.Type.Force where

import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer (WriterT (..))
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyExternal)
import Vehicle.Compile.Type.Meta (MetaSet, metaSolution)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (singleton)
import Vehicle.Compile.Type.Monad.Class
  ( MonadTypeChecker,
    getMetaInfo,
  )
import Vehicle.Data.Builtin.Interface.Blocked (BlockingStatus (..))
import Vehicle.Data.Builtin.Interface.Normalise
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
  (maybeForcedExpr, blockingMetas) <- forceExpr ctx expr
  forcedExpr <- case maybeForcedExpr of
    Nothing -> return expr
    Just forcedExpr -> do
      logDebug MaxDetail $
        "forced"
          <+> squotes (prettyExternal (WithContext expr ctx))
          <+> "to"
          <+> squotes (prettyExternal (WithContext forcedExpr ctx))
      return forcedExpr
  return (forcedExpr, blockingMetas)

-- | Recursively forces the evaluation of any meta-variables that are blocking
-- evaluation.
forceExpr ::
  (MonadForce builtin m) =>
  NamedBoundCtx ->
  Value builtin ->
  m (Maybe (Value builtin), MetaSet)
forceExpr ctx = \case
  VMeta m spine -> forceMeta ctx m spine
  VBuiltin b spine -> forceBuiltin ctx b spine
  _ -> return (Nothing, mempty)

forceMeta ::
  forall builtin m.
  (MonadForce builtin m) =>
  NamedBoundCtx ->
  MetaID ->
  Spine builtin ->
  m (Maybe (Value builtin), MetaSet)
forceMeta ctx m spine = do
  metaInfo <- getMetaInfo m
  case metaSolution metaInfo of
    Just solution -> do
      normMetaExpr <- normaliseApp ctx (normalised solution) spine
      (maybeForcedExpr, blockingMetas) <- forceExpr ctx normMetaExpr
      let forcedExpr = maybe (Just normMetaExpr) Just maybeForcedExpr
      return (forcedExpr, blockingMetas)
    Nothing -> return (Nothing, MetaSet.singleton m)

forceBuiltin ::
  (MonadForce builtin m) =>
  NamedBoundCtx ->
  builtin ->
  Spine builtin ->
  m (Maybe (Value builtin), MetaSet)
forceBuiltin ctx b spine = case blockingStatus b spine of
  Blocked traverseBlocking -> do
    (maybeUnblockedSpine, blockingMetas) <-
      runWriterT $ runMaybeT $ traverseBlocking $ forceBlockingArg ctx
    finalValue <- traverse (normaliseBuiltin ctx b) maybeUnblockedSpine
    return (finalValue, blockingMetas)
  _ -> return (Just (VBuiltin b spine), mempty)

forceBlockingArg ::
  (MonadForce builtin m) =>
  NamedBoundCtx ->
  Value builtin ->
  MaybeT (WriterT MetaSet m) (Value builtin)
forceBlockingArg ctx value = MaybeT $ WriterT $ forceExpr ctx value
