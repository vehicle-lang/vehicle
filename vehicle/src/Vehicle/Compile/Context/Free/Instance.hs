{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Context.Free.Instance where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), mapReaderT)
import Control.Monad.State
import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Vehicle.Compile.Context.Free.Class
import Vehicle.Compile.Context.Free.Core
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Normalise.Builtin (NormalisableBuiltin)
import Vehicle.Compile.Normalise.Monad
import Vehicle.Compile.Normalise.NBE
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin)
import Vehicle.Compile.Type.Core (mkTypingDeclCtxEntry)
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Free variable context monad instantiation

newtype FreeContextT builtin m a = FreeContextT
  { unFreeContextT :: ReaderT (FreeCtx builtin) m a
  }
  deriving (Functor, Applicative, Monad)

-- | Runs a computation in the context monad allowing you to keep track of the
-- context. Note that you must still call `addDeclToCtx` and `addBinderToCtx`
-- manually in the right places.
runFreeContextT :: (Monad m) => FreeCtx builtin -> FreeContextT builtin m a -> m a
runFreeContextT ctx (FreeContextT contextFn) = runReaderT contextFn ctx

-- | Runs a computation in the context monad allowing you to keep track of the
-- context. Note that you must still call `addDeclToCtx` and `addBinderToCtx`
-- manually in the right places.
runFreshFreeContextT :: (Monad m) => Proxy builtin -> FreeContextT builtin m a -> m a
runFreshFreeContextT _ = runFreeContextT mempty

instance MonadTrans (FreeContextT builtin) where
  lift = FreeContextT . lift

mapFreeContextT ::
  (m a -> n b) ->
  FreeContextT builtin m a ->
  FreeContextT builtin n b
mapFreeContextT f m = FreeContextT (mapReaderT f (unFreeContextT m))

--------------------------------------------------------------------------------
-- Other monad preservation

instance (MonadLogger m) => MonadLogger (FreeContextT builtin m) where
  setCallDepth = FreeContextT . setCallDepth
  getCallDepth = FreeContextT getCallDepth
  incrCallDepth = FreeContextT incrCallDepth
  decrCallDepth = FreeContextT decrCallDepth
  getDebugLevel = FreeContextT getDebugLevel
  logMessage = FreeContextT . logMessage

instance (MonadError e m) => MonadError e (FreeContextT builtin m) where
  throwError = lift . throwError
  catchError m f = FreeContextT (catchError (unFreeContextT m) (unFreeContextT . f))

instance (MonadState s m) => MonadState s (FreeContextT builtin m) where
  get = lift get
  put = lift . put

instance (MonadReader s m) => MonadReader s (FreeContextT builtin m) where
  ask = lift ask
  local = mapFreeContextT . local

instance (MonadIO m) => MonadIO (FreeContextT builtin m) where
  liftIO = lift . liftIO

--------------------------------------------------------------------------------
-- Context monad preservation

normaliseInEnv ::
  forall builtin m.
  (MonadFreeContext builtin m) =>
  Env builtin ->
  Expr Ix builtin ->
  m (Value builtin)
normaliseInEnv env e = do
  declCtx <- getFreeCtx (Proxy @builtin)
  let normDeclCtx = fmap mkTypingDeclCtxEntry declCtx
  runNormT defaultEvalOptions normDeclCtx (eval env e)

normaliseInEmptyEnv ::
  (MonadFreeContext builtin m) =>
  Expr Ix builtin ->
  m (Value builtin)
normaliseInEmptyEnv = normaliseInEnv mempty

glue :: (MonadFreeContext builtin m) => Env builtin -> Expr Ix builtin -> m (GluedExpr builtin)
glue env e = Glued e <$> normaliseInEnv env e

instance
  (PrintableBuiltin builtin, NormalisableBuiltin builtin, MonadCompile m) =>
  MonadFreeContext builtin (FreeContextT builtin m)
  where
  addDeclToContext decl cont = do
    gluedDecl <- traverse (\e -> Glued e <$> normaliseInEmptyEnv e) decl
    FreeContextT $ do
      let updateCtx = Map.insert (identifierOf decl) gluedDecl
      local updateCtx (unFreeContextT cont)

  getFreeCtx _ = FreeContextT ask
