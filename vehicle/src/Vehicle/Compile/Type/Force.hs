module Vehicle.Compile.Type.Force where

import Data.Maybe (fromMaybe, isJust)
import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Monad
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap (lookup)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (singleton, unions)
import Vehicle.Expr.BuiltinInterface (HasStandardData (getBuiltinFunction))
import Vehicle.Expr.Normalised

-----------------------------------------------------------------------------
-- Meta-variable forcing

-- | Recursively forces the evaluation of any meta-variables at the head
-- of the expresson.
forceHead ::
  (MonadNorm builtin m) =>
  ConstraintContext builtin ->
  Value builtin ->
  m (Value builtin, MetaSet)
forceHead ctx expr = do
  (maybeForcedExpr, blockingMetas) <- forceExpr expr
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
  (MonadNorm builtin m) =>
  Value builtin ->
  m (Maybe (Value builtin), MetaSet)
forceExpr = go
  where
    go :: Value builtin -> m (Maybe (Value builtin), MetaSet)
    go = \case
      VMeta m spine -> goMeta m spine
      VBuiltin b spine -> forceBuiltin b spine
      _ -> return (Nothing, mempty)

    goMeta :: MetaID -> Spine builtin -> m (Maybe (Value builtin), MetaSet)
    goMeta m spine = do
      metaSubst <- getMetaSubstitution
      case MetaMap.lookup m metaSubst of
        Just solution -> do
          normMetaExpr <- evalApp (normalised solution) spine
          (maybeForcedExpr, blockingMetas) <- go normMetaExpr
          let forcedExpr = maybe (Just normMetaExpr) Just maybeForcedExpr
          return (forcedExpr, blockingMetas)
        Nothing -> return (Nothing, MetaSet.singleton m)

forceArg :: (MonadNorm builtin m) => VArg builtin -> m (VArg builtin, (Bool, MetaSet))
forceArg arg = do
  (maybeResult, blockingMetas) <- unpairArg <$> traverse forceExpr arg
  let result = fmap (fromMaybe (argExpr arg)) maybeResult
  let reduced = isJust $ argExpr maybeResult
  return (result, (reduced, blockingMetas))

forceBuiltin ::
  (MonadNorm builtin m) =>
  builtin ->
  Spine builtin ->
  m (Maybe (Value builtin), MetaSet)
forceBuiltin b spine = case getBuiltinFunction b of
  Nothing -> return (Nothing, mempty)
  Just {} -> do
    (argResults, argData) <- unzip <$> traverse forceArg spine
    let (argsReduced, argBlockingMetas) = unzip argData
    let anyArgsReduced = or argsReduced
    let blockingMetas = MetaSet.unions argBlockingMetas
    result <-
      if not anyArgsReduced
        then return Nothing
        else do
          Just <$> evalBuiltin evalApp b argResults
    return (result, blockingMetas)
