{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging.Instance
  ( LoggerT (..),
    LoggingSettings (..),
    ImmediateLoggerT,
    runImmediateLoggerT,
    SilentLoggerT,
    SilentLogger,
    runSilentLoggerT,
    DelayedLoggerT,
    DelayedLogger,
    runDelayedLoggerT,
    showCompileWarnings,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.RWS (RWST (..))
import Control.Monad.Reader (ask)
import Control.Monad.State (MonadState (..), modify)
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Control.Monad.Writer (MonadWriter (..))
import Data.Text (Text, pack)
import System.Console.ANSI (Color (..))
import Vehicle.Compile.Print.Warning ()
import Vehicle.Prelude
import Vehicle.Prelude.IO as VIO (MonadStdIO (..))
import Vehicle.Prelude.Logging.Backend
import Vehicle.Prelude.Logging.Class
import Vehicle.Prelude.Warning

--------------------------------------------------------------------------------
-- Settings

data LoggingSettings = LoggingSettings
  { putLogLn :: Text -> IO (),
    loggingLevel :: LoggingLevel
  }

--------------------------------------------------------------------------------
-- Logging monad instance

newtype LoggerT m a = LoggerT
  { unloggerT :: RWST LoggingLevel [CompileWarning] Int m a
  }
  deriving (Functor, Applicative, Monad)

runLoggerT :: (Monad m) => LoggingLevel -> LoggerT m a -> m (a, [SummarisedCompileWarning])
runLoggerT debugLevel (LoggerT logger) = do
  (result, _, warnings) <- runRWST logger debugLevel 0
  return (result, groupWarnings warnings)

instance (Monad m, MonadLoggingBackend m) => MonadLogger (LoggerT m) where
  setCallDepth = LoggerT . put
  getCallDepth = LoggerT get
  incrCallDepth = LoggerT $ modify (+ 1)
  decrCallDepth = LoggerT $ modify (\x -> x - 1)
  getDebugLevel = LoggerT ask
  logMessage m = LoggerT $ logDebugMessage m
  logWarning m = LoggerT $ tell [m]

instance MonadTrans LoggerT where
  lift = LoggerT . lift

instance (MonadError e m) => MonadError e (LoggerT m) where
  throwError = lift . throwError
  catchError m f = LoggerT (catchError (unloggerT m) (unloggerT . f))

instance (MonadIO m) => MonadIO (LoggerT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (LoggerT m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

--------------------------------------------------------------------------------
-- Immediate logging

-- | Immediately prints the logged messages to the output handle.
type ImmediateLoggerT m = LoggerT (ImmediateBackendT m)

runImmediateLoggerT :: (MonadIO m) => LoggingSettings -> ImmediateLoggerT m a -> m a
runImmediateLoggerT LoggingSettings {..} value = do
  (result, warnings) <- runImmediateBackendT putLogLn (runLoggerT loggingLevel value)
  unless (null warnings) $
    liftIO $
      putLogLn $
        pack $
          showCompileWarnings warnings
  return result

--------------------------------------------------------------------------------
-- Silent logging

-- | Immediately prints the logged messages to the output handle.
type SilentLoggerT m = LoggerT (SilentBackendT m)

type SilentLogger = SilentLoggerT Identity

runSilentLoggerT :: (Monad m) => SilentLoggerT m a -> m a
runSilentLoggerT value = fst <$> runSilentBackendT (runLoggerT NoDetail value)

--------------------------------------------------------------------------------
-- Delayed logging

-- | Stores the messages but doesn't print them immediately.
type DelayedLoggerT m = LoggerT (DelayedBackendT m)

type DelayedLogger = DelayedLoggerT Identity

runDelayedLoggerT :: (Monad m) => LoggingLevel -> DelayedLoggerT m a -> m (a, [DebugMessage], [SummarisedCompileWarning])
runDelayedLoggerT loggingLevel value = do
  ((a, w), m) <- runDelayedBackendT (runLoggerT loggingLevel value)
  return (a, m, w)

--------------------------------------------------------------------------------
-- Showing warnings

showCompileWarnings :: [SummarisedCompileWarning] -> String
showCompileWarnings ws =
  setTextColour Yellow $
    layoutAsString $
      vsep (fmap (\w -> line <> "Warning: " <+> pretty w <> line) ws)
