{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Compile.Context.Bound.Instance where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks, mapReaderT)
import Control.Monad.State (MonadState (..))
import Control.Monad.Writer
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Proxy)
import Vehicle.Compile.Context.Bound.Class
import Vehicle.Compile.Context.Bound.Core
import Vehicle.Compile.Error (MonadCompile)
import Vehicle.Compile.Prelude

--------------------------------------------------------------------------------
-- Context monad instantiation

newtype BoundContextT expr m a = BoundContextT
  { unBoundContextT :: ReaderT (BoundCtx expr, FreshNameState) m a
  }
  deriving (Functor, Applicative, Monad)

-- | Runs a computation in the context monad allowing you to keep track of the
-- context. Note that you must still call `addDeclToCtx` and `addBinderToCtx`
-- manually in the right places.
runBoundContextT :: (Monad m) => BoundCtx expr -> BoundContextT expr m a -> m a
runBoundContextT ctx (BoundContextT contextFn) = runReaderT contextFn (ctx, 0)

-- | Runs a computation in the context monad allowing you to keep track of the
-- context. Note that you must still call `addDeclToCtx` and `addBinderToCtx`
-- manually in the right places.
runFreshBoundContextT :: (Monad m) => Proxy expr -> BoundContextT expr m a -> m a
runFreshBoundContextT _ = runBoundContextT mempty

instance MonadTrans (BoundContextT expr) where
  lift = BoundContextT . lift

mapBoundContextT ::
  (m a -> n b) ->
  BoundContextT expr m a ->
  BoundContextT expr n b
mapBoundContextT f m = BoundContextT (mapReaderT f (unBoundContextT m))

--------------------------------------------------------------------------------
-- Other monad preservation

instance (MonadLogger m) => MonadLogger (BoundContextT expr m) where
  setCallDepth = BoundContextT . setCallDepth
  getCallDepth = BoundContextT getCallDepth
  incrCallDepth = BoundContextT incrCallDepth
  decrCallDepth = BoundContextT decrCallDepth
  getDebugLevel = BoundContextT getDebugLevel
  logMessage = BoundContextT . logMessage
  logWarning = BoundContextT . logWarning

instance (MonadError e m) => MonadError e (BoundContextT expr m) where
  throwError = lift . throwError
  catchError m f = BoundContextT (catchError (unBoundContextT m) (unBoundContextT . f))

instance (MonadState s m) => MonadState s (BoundContextT expr m) where
  get = lift get
  put = lift . put

instance (MonadReader s m) => MonadReader s (BoundContextT expr m) where
  ask = lift ask
  local = mapBoundContextT . local

instance (MonadCompile m) => MonadBoundContext expr (BoundContextT expr m) where
  addBinderToContext binder cont = BoundContextT $ do
    local (first (binder :)) (unBoundContextT cont)

  getBoundCtx _ = BoundContextT $ asks fst
