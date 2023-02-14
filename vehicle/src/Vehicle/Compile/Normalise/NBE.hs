{-# HLINT ignore "Use <|>" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.NBE
  ( whnf,
    eval,
    evalApp,
    liftEnvOverBinder,
    forceHead,
    forceArg,
    reeval,
    MonadNorm,
    NormT,
    runNormT,
    runEmptyNormT,
    NormalisableBuiltin (..),
    InternalMonadNorm (..),
  )
where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (WriterT)
import Data.List.NonEmpty as NonEmpty (toList)
import Data.Map qualified as Map (lookup)
import Data.Maybe (isJust)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Map qualified as MetaMap
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet
import Vehicle.Compile.Type.VariableContext
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

-----------------------------------------------------------------------------
-- Main method

whnf ::
  MonadNorm builtin m =>
  DBLevel ->
  DBExpr builtin ->
  m (NormExpr builtin)
whnf dbLevel = eval (mkNoOpEnv dbLevel)

-----------------------------------------------------------------------------
-- Normalisation monad

class MonadCompile m => InternalMonadNorm builtin m where
  getDeclSubstitution :: m (DeclSubstitution builtin)
  getMetaSubstitution :: m (MetaSubstitution builtin)

instance InternalMonadNorm builtin m => InternalMonadNorm builtin (StateT s m) where
  getDeclSubstitution = lift getDeclSubstitution
  getMetaSubstitution = lift getMetaSubstitution

instance (Monoid s, InternalMonadNorm builtin m) => InternalMonadNorm builtin (WriterT s m) where
  getDeclSubstitution = lift getDeclSubstitution
  getMetaSubstitution = lift getMetaSubstitution

instance InternalMonadNorm builtin m => InternalMonadNorm builtin (ReaderT s m) where
  getDeclSubstitution = lift getDeclSubstitution
  getMetaSubstitution = lift getMetaSubstitution

newtype NormT builtin m a = NormT
  { unnormT :: ReaderT (DeclSubstitution builtin, MetaSubstitution builtin) m a
  }
  deriving (Functor, Applicative, Monad)

runNormT :: DeclSubstitution builtin -> MetaSubstitution builtin -> NormT builtin m a -> m a
runNormT declSubst metaSubst x = runReaderT (unnormT x) (declSubst, metaSubst)

runEmptyNormT :: NormT builtin m a -> m a
runEmptyNormT = runNormT mempty mempty

instance MonadTrans (NormT builtin) where
  lift = NormT . lift

instance MonadLogger m => MonadLogger (NormT builtin m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance MonadError e m => MonadError e (NormT builtin m) where
  throwError = lift . throwError
  catchError m f = NormT (catchError (unnormT m) (unnormT . f))

instance MonadCompile m => InternalMonadNorm builtin (NormT builtin m) where
  getDeclSubstitution = NormT $ asks fst
  getMetaSubstitution = NormT $ asks snd

-----------------------------------------------------------------------------
-- Subsystem

class (Eq builtin, PrintableBuiltin builtin) => NormalisableBuiltin builtin where
  -- | The ability to evaluate the builtin
  evalBuiltin ::
    InternalMonadNorm builtin m =>
    builtin ->
    Spine builtin ->
    m (NormExpr builtin)

  -- | Force the application of a builtin returning the reduced expression and the
  -- set of blocking metas.
  forceBuiltin ::
    InternalMonadNorm builtin m =>
    builtin ->
    Spine builtin ->
    m (Maybe (NormExpr builtin), MetaSet)

type MonadNorm builtin m =
  ( InternalMonadNorm builtin m,
    NormalisableBuiltin builtin
  )

-----------------------------------------------------------------------------
-- Evaluation

-- TODO change to return a tuple of NF and WHNF?
eval :: MonadNorm builtin m => Env builtin -> DBExpr builtin -> m (NormExpr builtin)
eval env expr = do
  showEntry env expr
  result <- case expr of
    Hole {} -> resolutionError currentPass "Hole"
    Meta _ m -> return $ VMeta m []
    Universe _ u -> return $ VUniverse u
    Builtin _ b -> return $ VBuiltin b []
    Literal _ l -> return $ VLiteral l
    Ann _ e _ -> eval env e
    LVec _ xs -> VLVec <$> traverse (eval env) xs <*> pure []
    Lam _ binder body -> do
      binder' <- evalBinder env binder
      return $ VLam binder' env body
    Pi _ binder body ->
      VPi <$> evalBinder env binder <*> eval (liftEnvOverBinder env) body
    Var _ v -> case v of
      Bound i -> i `lookupIn` env
      Free ident -> do
        declSubst <- getDeclSubstitution
        let declExpr = Map.lookup ident declSubst
        return $ case declExpr of
          Just x -> x
          Nothing -> VFreeVar ident []
    Let _ bound _binder body -> do
      boundNormExpr <- eval env bound
      eval (boundNormExpr : env) body
    App _ fun args -> do
      fun' <- eval env fun
      args' <- traverse (traverse (eval env)) args
      evalApp fun' (NonEmpty.toList args')

  showExit result
  return result

evalBinder :: MonadNorm builtin m => Env builtin -> DBBinder builtin -> m (NormBinder builtin)
evalBinder env = traverse (eval env)

evalApp :: MonadNorm builtin m => NormExpr builtin -> Spine builtin -> m (NormExpr builtin)
evalApp fun [] = return fun
evalApp fun (arg : args) = case fun of
  VMeta v spine -> return $ VMeta v (spine <> (arg : args))
  VFreeVar v spine -> return $ VFreeVar v (spine <> (arg : args))
  VBoundVar v spine -> return $ VBoundVar v (spine <> (arg : args))
  VLVec xs spine -> return $ VLVec xs (spine <> (arg : args))
  VLam _binder env body -> do
    body' <- eval (argExpr arg : env) body
    case args of
      [] -> return body'
      (a : as) -> evalApp body' (a : as)
  VBuiltin b spine -> evalBuiltin b (spine <> (arg : args))
  VUniverse {} -> unexpectedExprError currentPass "VUniverse"
  VPi {} -> unexpectedExprError currentPass "VPi"
  VLiteral {} -> unexpectedExprError currentPass "VLiteral"

lookupIn :: MonadCompile m => DBIndex -> Env builtin -> m (NormExpr builtin)
lookupIn i env = case lookupVar env i of
  Just value -> return value
  Nothing ->
    compilerDeveloperError $
      "Environment of size"
        <+> pretty (length env)
        <+> "in which NBE is being performed"
        <+> "is smaller than the found DB index"
        <+> pretty i

-----------------------------------------------------------------------------
-- Reevaluation

reeval :: MonadNorm builtin m => NormExpr builtin -> m (NormExpr builtin)
reeval expr = case expr of
  VUniverse {} -> return expr
  VLiteral {} -> return expr
  VLam {} -> return expr
  VPi {} -> return expr
  VLVec xs spine -> VLVec <$> traverse reeval xs <*> reevalSpine spine
  VMeta m spine -> VMeta m <$> reevalSpine spine
  VFreeVar v spine -> VFreeVar v <$> reevalSpine spine
  VBoundVar v spine -> VBoundVar v <$> reevalSpine spine
  VBuiltin b spine -> evalApp (VBuiltin b []) =<< reevalSpine spine

reevalSpine :: MonadNorm builtin m => Spine builtin -> m (Spine builtin)
reevalSpine = traverse (traverse reeval)

-----------------------------------------------------------------------------
-- Meta-variable forcing

-- | Recursively forces the evaluation of any meta-variables at the head
-- of the expresson.
forceHead :: MonadNorm builtin m => NormExpr builtin -> m (NormExpr builtin, MetaSet)
forceHead expr = do
  (maybeForcedExpr, blockingMetas) <- forceExpr expr
  forcedExpr <- case maybeForcedExpr of
    Nothing -> return expr
    Just forcedExpr -> do
      logDebug MaxDetail $ "forced" <+> prettyVerbose expr <+> "to" <+> prettyVerbose forcedExpr
      return forcedExpr
  return (forcedExpr, blockingMetas)

-- | Recursively forces the evaluation of any meta-variables that are blocking
-- evaluation.
forceExpr :: forall builtin m. MonadNorm builtin m => NormExpr builtin -> m (Maybe (NormExpr builtin), MetaSet)
forceExpr = go
  where
    go :: NormExpr builtin -> m (Maybe (NormExpr builtin), MetaSet)
    go = \case
      VMeta m spine -> goMeta m spine
      VBuiltin b spine -> forceBuiltin b spine
      _ -> return (Nothing, mempty)

    goMeta :: MetaID -> Spine builtin -> m (Maybe (NormExpr builtin), MetaSet)
    goMeta m spine = do
      metaSubst <- getMetaSubstitution
      case MetaMap.lookup m metaSubst of
        Just solution -> do
          normMetaExpr <- evalApp (normalised solution) spine
          (maybeForcedExpr, blockingMetas) <- go normMetaExpr
          let forcedExpr = maybe (Just normMetaExpr) Just maybeForcedExpr
          return (forcedExpr, blockingMetas)
        Nothing -> return (Nothing, MetaSet.singleton m)

forceArg :: MonadNorm builtin m => NormArg builtin -> m (NormArg builtin, Bool, MetaSet)
forceArg arg
  -- We assume that non-explicit args aren't depended on computationally
  -- (this may not hold in future.)
  | not (isExplicit arg) = return (arg, False, mempty)
  | otherwise = do
      (maybeResult, blockingMetas) <- forceExpr (argExpr arg)
      let result = maybe arg (`replaceArgExpr` arg) maybeResult
      let reduced = isJust maybeResult
      return (result, reduced, blockingMetas)

-----------------------------------------------------------------------------
-- Other

currentPass :: Doc ()
currentPass = "normalisation by evaluation"

showEntry :: MonadNorm builtin m => Env builtin -> DBExpr builtin -> m ()
showEntry _env _expr = do
  -- logDebug MaxDetail $ "nbe-entry" <+> prettyVerbose expr -- <+> "   { env=" <+> prettyVerbose env <+> "}")
  incrCallDepth

showExit :: MonadNorm builtin m => NormExpr builtin -> m ()
showExit _result = do
  decrCallDepth

-- logDebug  MaxDetail ("nbe-exit" <+> prettyVerbose result)
