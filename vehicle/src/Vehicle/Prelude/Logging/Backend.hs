{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging.Backend
  ( MonadLoggingBackend (..),
    ImmediateBackendT,
    runImmediateBackendT,
    SilentBackendT,
    runSilentBackendT,
    DelayedBackendT,
    runDelayedBackendT,
  )
where

import Control.Monad.Identity (IdentityT (..))
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State (StateT (..), evalStateT)
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Text qualified as Text (pack)
import Vehicle.Compile.Print.Warning ()
import Vehicle.Prelude.IO as VIO (MonadStdIO (..))
import Vehicle.Prelude.Logging.Class

--------------------------------------------------------------------------------
-- Class for logging backends

class (Monad m) => MonadLoggingBackend m where
  logDebugMessage :: DebugMessage -> m ()

instance (MonadLoggingBackend m, Monoid w) => MonadLoggingBackend (RWST r w s m) where
  logDebugMessage = lift . logDebugMessage

--------------------------------------------------------------------------------
-- Immediate backend

type LogAction = Text -> IO ()

newtype ImmediateBackendT m a = ImmediateBackendT
  { unImmediateBackendT :: StateT IntSet (ReaderT LogAction m) a
  }
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadLoggingBackend (ImmediateBackendT m) where
  logDebugMessage message = ImmediateBackendT $ do
    logAction <- ask
    lift $ liftIO $ logAction (Text.pack $ show message)

instance MonadTrans ImmediateBackendT where
  lift = ImmediateBackendT . lift . lift

instance (MonadIO m) => MonadIO (ImmediateBackendT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (ImmediateBackendT m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

runImmediateBackendT :: (MonadIO m) => (Text -> IO ()) -> ImmediateBackendT m a -> m a
runImmediateBackendT putLogLn v =
  runReaderT (evalStateT (unImmediateBackendT v) mempty) putLogLn

--------------------------------------------------------------------------------
-- Silent backend

type SilentBackendT = IdentityT

instance (Monad m) => MonadLoggingBackend (SilentBackendT m) where
  logDebugMessage _message = return ()

runSilentBackendT :: SilentBackendT m a -> m a
runSilentBackendT = runIdentityT

--------------------------------------------------------------------------------
-- Delayed backend

newtype DelayedBackendT m a = DelayedBackendT
  { unDelayedBackendT :: StateT IntSet (WriterT [DebugMessage] m) a
  }
  deriving (Functor, Applicative, Monad)

instance (Monad m) => MonadLoggingBackend (DelayedBackendT m) where
  logDebugMessage message = DelayedBackendT $ tell [message]

instance MonadTrans DelayedBackendT where
  lift = DelayedBackendT . lift . lift

instance (MonadIO m) => MonadIO (DelayedBackendT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (DelayedBackendT m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

runDelayedBackendT :: (Monad m) => DelayedBackendT m a -> m (a, [DebugMessage])
runDelayedBackendT v = runWriterT (evalStateT (unDelayedBackendT v) mempty)
