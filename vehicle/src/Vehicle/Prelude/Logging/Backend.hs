{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging.Backend
  ( MonadLoggingBackend(..)
  , ImmediateBackendT
  , runImmediateBackendT
  , SilentBackendT
  , runSilentBackendT
  , DelayedBackendT
  , runDelayedBackendT
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State (StateT (..))
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Control.Monad.Writer (MonadWriter(..),WriterT(..))
import System.IO (Handle, hPrint)

import Vehicle.Prelude.Logging.Class

--------------------------------------------------------------------------------
-- Class for logging backends

class Monad m => MonadLoggingBackend m where
  output :: Message -> m ()

instance MonadLoggingBackend m => MonadLoggingBackend (StateT s m) where
  output = lift . output

instance MonadLoggingBackend m => MonadLoggingBackend (ReaderT s m) where
  output = lift . output

--------------------------------------------------------------------------------
-- Immediate backend

newtype ImmediateBackendT m a = ImmediateBackendT
  { unImmediateBackendT :: ReaderT Handle m a
  } deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadLoggingBackend (ImmediateBackendT m) where
  output message = ImmediateBackendT $ do
    handle <- ask
    lift $ liftIO $ hPrint handle message

instance MonadTrans ImmediateBackendT where
  lift = ImmediateBackendT . lift

instance MonadIO m => MonadIO (ImmediateBackendT m) where
  liftIO = lift . liftIO

runImmediateBackendT :: MonadIO m => Handle -> ImmediateBackendT m a -> m a
runImmediateBackendT logHandle v = runReaderT (unImmediateBackendT v) logHandle

--------------------------------------------------------------------------------
-- Silent backend

newtype SilentBackendT m a = SilentBackendT
  { unSilentBackendT :: m a
  } deriving (Functor, Applicative, Monad)

instance Monad m => MonadLoggingBackend (SilentBackendT m) where
  output _message = return ()

instance MonadTrans SilentBackendT where
  lift = SilentBackendT

instance MonadIO m => MonadIO (SilentBackendT m) where
  liftIO = lift . liftIO

instance MonadError e m => MonadError e (SilentBackendT m) where
  throwError     = lift . throwError
  catchError m f = SilentBackendT (catchError (unSilentBackendT m) (unSilentBackendT . f))

runSilentBackendT :: SilentBackendT m a -> m a
runSilentBackendT = unSilentBackendT

--------------------------------------------------------------------------------
-- Delayed backend

newtype DelayedBackendT m a = DelayedBackendT
  { unDelayedBackendT :: WriterT [Message] m a
  } deriving (Functor, Applicative, Monad)

instance Monad m => MonadLoggingBackend (DelayedBackendT m) where
  output m = DelayedBackendT $ tell [m]

runDelayedBackendT :: DelayedBackendT m a -> m (a, [Message])
runDelayedBackendT v = runWriterT (unDelayedBackendT v)
