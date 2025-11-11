{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vehicle.Data.Variable.Bound.Context.Name.Instance where

-- Simple module that specialises MonadBoundContext for the common occurence
-- where you only need to know the bound variable's names.

import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), mapReaderT)
import Control.Monad.State (MonadState (..))
import Control.Monad.Writer
import Vehicle.Compile.Prelude
import Vehicle.Data.Variable.Bound.Context.Name.Class
import Vehicle.Data.Variable.Bound.Context.Name.Core

--------------------------------------------------------------------------------
-- Context monad instantiation

newtype NameBoundContextT m a = NameBoundContextT
  { unNameBoundContextT :: ReaderT NamedBoundCtx m a
  }
  deriving (Functor, Applicative, Monad)

type BoundContext a = NameBoundContextT Identity a

-- | Runs a computation in the context monad allowing you to keep track of the
-- context. Note that you must still call `addDeclToCtx` and `addBinderToCtx`
-- manually in the right places.
runNameBoundContextT :: (Monad m) => NamedBoundCtx -> NameBoundContextT m a -> m a
runNameBoundContextT ctx (NameBoundContextT contextFn) = runReaderT contextFn ctx

runNameBoundContext :: NamedBoundCtx -> BoundContext a -> a
runNameBoundContext ctx fn = runIdentity $ runNameBoundContextT ctx fn

-- | Runs a computation in the context monad allowing you to keep track of the
-- context. Note that you must still call `addDeclToCtx` and `addBinderToCtx`
-- manually in the right places.
runFreshNameBoundContextT :: (Monad m) => NameBoundContextT m a -> m a
runFreshNameBoundContextT = runNameBoundContextT mempty

runFreshNameBoundContext :: BoundContext a -> a
runFreshNameBoundContext fn = runIdentity $ runFreshNameBoundContextT fn

mapNameBoundContextT ::
  (m a -> n b) ->
  NameBoundContextT m a ->
  NameBoundContextT n b
mapNameBoundContextT f m = NameBoundContextT (mapReaderT f (unNameBoundContextT m))

--------------------------------------------------------------------------------
-- Other monad preservation

instance MonadTrans NameBoundContextT where
  lift = NameBoundContextT . lift

instance (MonadLogger m) => MonadLogger (NameBoundContextT m) where
  setCallDepth = NameBoundContextT . setCallDepth
  getCallDepth = NameBoundContextT getCallDepth
  incrCallDepth = NameBoundContextT incrCallDepth
  decrCallDepth = NameBoundContextT decrCallDepth
  getDebugLevel = NameBoundContextT getDebugLevel
  logMessage = NameBoundContextT . logMessage
  logWarning = NameBoundContextT . logWarning
  enterCompilerPass = NameBoundContextT . enterCompilerPass
  exitCompilerPass = NameBoundContextT exitCompilerPass

instance (MonadError e m) => MonadError e (NameBoundContextT m) where
  throwError = lift . throwError
  catchError m f = NameBoundContextT (catchError (unNameBoundContextT m) (unNameBoundContextT . f))

instance (MonadState s m) => MonadState s (NameBoundContextT m) where
  get = lift get
  put = lift . put

instance (MonadReader s m) => MonadReader s (NameBoundContextT m) where
  ask = lift ask
  local = mapNameBoundContextT . local

instance (Monad m) => MonadReadableNameContext (NameBoundContextT m) where
  getNameContext = NameBoundContextT ask

instance (Monad m) => MonadNameContext (NameBoundContextT m) where
  addNameToContext binder cont = NameBoundContextT $ do
    local (nameOf binder :) (unNameBoundContextT cont)
