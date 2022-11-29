{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging.Instance
  ( LoggerT(..)
  , LoggingSettings(..)
  , ImmediateLoggerT
  , ImmediateLogger
  , runImmediateLoggerT
  , runImmediateLogger
  , SilentLoggerT
  , runSilentLoggerT
  , runSilentLogger
  , DelayedLoggerT
  , DelayedLogger
  , runDelayedLoggerT
  , runDelayedLogger
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..), ask)
import Control.Monad.State (StateT (..), evalStateT, get, modify)
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Control.Monad.Identity (Identity(..))
import System.IO (Handle)

import Vehicle.Prelude.Logging.Class
import Vehicle.Prelude.Logging.Backend

--------------------------------------------------------------------------------
-- Settings

data LoggingSettings = LoggingSettings
  { logHandle    :: Handle
  , loggingLevel :: LoggingLevel
  }

--------------------------------------------------------------------------------
-- Logging monad instance

newtype LoggerT m a = LoggerT
  { unloggerT :: ReaderT LoggingLevel (StateT Int m) a
  } deriving (Functor, Applicative, Monad)

runLoggerT :: Monad m => LoggingLevel -> LoggerT m a -> m a
runLoggerT debugLevel (LoggerT logger) =
  evalStateT (runReaderT logger debugLevel) 0

instance (Monad m, MonadLoggingBackend m) => MonadLogger (LoggerT m) where
  getCallDepth  = LoggerT get
  incrCallDepth = LoggerT $ modify (+1)
  decrCallDepth = LoggerT $ modify (\x -> x-1)
  getDebugLevel = LoggerT ask
  logMessage m  = LoggerT $ output m

instance MonadTrans LoggerT where
  lift = LoggerT . lift . lift

instance MonadError e m => MonadError e (LoggerT m) where
  throwError     = lift . throwError
  catchError m f = LoggerT (catchError (unloggerT m) (unloggerT . f))

instance MonadIO m => MonadIO (LoggerT m) where
  liftIO = lift . liftIO

--------------------------------------------------------------------------------
-- Immediate logging

-- | Immediately prints the logged messages to the output handle.
type ImmediateLoggerT m = LoggerT (ImmediateBackendT m)

type ImmediateLogger = ImmediateLoggerT IO

runImmediateLoggerT :: MonadIO m => LoggingSettings -> ImmediateLoggerT m a -> m a
runImmediateLoggerT LoggingSettings{..} value =
  runImmediateBackendT logHandle (runLoggerT loggingLevel value)

runImmediateLogger :: MonadIO m => LoggingSettings -> ImmediateLoggerT m a -> m a
runImmediateLogger LoggingSettings{..} value =
  runImmediateBackendT logHandle (runLoggerT loggingLevel value)

--------------------------------------------------------------------------------
-- Silent logging

-- | Immediately prints the logged messages to the output handle.
type SilentLoggerT m = LoggerT (SilentBackendT m)

type SilentLogger = SilentLoggerT Identity

runSilentLoggerT :: Monad m => SilentLoggerT m a -> m a
runSilentLoggerT value = runSilentBackendT (runLoggerT NoDetail value)

runSilentLogger :: SilentLogger a -> a
runSilentLogger value = runIdentity $ runSilentLoggerT value

--------------------------------------------------------------------------------
-- Delayed logging

-- | Stores the messages but doesn't print them immediately.
type DelayedLoggerT m = LoggerT (DelayedBackendT m)

type DelayedLogger = DelayedLoggerT Identity

runDelayedLoggerT :: Monad m => LoggingLevel -> DelayedLoggerT m a -> m (a, [Message])
runDelayedLoggerT debugLevel = runDelayedBackendT . runLoggerT debugLevel

runDelayedLogger :: LoggingLevel -> DelayedLogger a -> (a, [Message])
runDelayedLogger debugLevel = runIdentity . runDelayedBackendT . runLoggerT debugLevel
