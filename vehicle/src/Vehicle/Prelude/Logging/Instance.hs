{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging.Instance
  ( LoggerT (..),
    LoggingSettings (..),
    runLoggerT,
    runSilentLoggerT,
    SilentLoggerT,
    showCompileWarnings,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.RWS (RWST (..))
import Control.Monad.Reader (asks)
import Control.Monad.State (MonadState (..), modify)
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Control.Monad.Writer (MonadWriter (..), WriterT (..))
import Data.Text (Text, pack)
import System.Console.ANSI (Color (..))
import Vehicle.Compile.Print.Warning ()
import Vehicle.Prelude
import Vehicle.Prelude.IO as VIO (MonadStdIO (..))
import Vehicle.Prelude.Logging.Class
import Vehicle.Prelude.Warning

--------------------------------------------------------------------------------
-- Settings

data LoggingSettings = LoggingSettings
  { putLogLn :: Text -> IO (),
    loggingLevel :: LoggingLevel
  }

--------------------------------------------------------------------------------
-- Standard logging monad

newtype LoggerT m a = LoggerT
  { unLoggerT :: RWST LoggingSettings [CompileWarning] Int m a
  }
  deriving (Functor, Applicative, Monad)

instance (MonadIO m) => MonadLogger (LoggerT m) where
  setCallDepth = LoggerT . put
  getCallDepth = LoggerT get
  incrCallDepth = LoggerT $ modify (+ 1)
  decrCallDepth = LoggerT $ modify (\x -> x - 1)
  getDebugLevel = LoggerT $ asks loggingLevel
  logWarning w = LoggerT $ tell [w]
  logMessage message = LoggerT $ do
    logAction <- asks putLogLn
    lift $ liftIO $ logAction (pack $ show message)

instance MonadTrans LoggerT where
  lift = LoggerT . lift

instance (MonadError e m) => MonadError e (LoggerT m) where
  throwError = lift . throwError
  catchError m f = LoggerT (catchError (unLoggerT m) (unLoggerT . f))

instance (MonadIO m) => MonadIO (LoggerT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (LoggerT m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

runLoudLoggerT :: (MonadIO m) => LoggingSettings -> LoggerT m a -> m (a, [CompileWarning])
runLoudLoggerT loggingSettings (LoggerT value) = do
  (result, _, warnings) <- runRWST value loggingSettings 0
  return (result, warnings)

--------------------------------------------------------------------------------
-- No logging

newtype SilentLoggerT m a = SilentLoggerT
  { unSilentLoggerT :: WriterT [CompileWarning] m a
  }
  deriving (Functor, Applicative, Monad)

instance MonadTrans SilentLoggerT where
  lift = SilentLoggerT . lift

instance (Monad m) => MonadLogger (SilentLoggerT m) where
  logMessage _message = return ()
  getCallDepth = return 0
  setCallDepth _ = return ()
  incrCallDepth = return ()
  decrCallDepth = return ()
  getDebugLevel = return NoDetail
  logWarning w = SilentLoggerT $ tell [w]

instance (MonadIO m) => MonadIO (SilentLoggerT m) where
  liftIO = lift . liftIO

instance (MonadStdIO m) => MonadStdIO (SilentLoggerT m) where
  writeStdout = lift . VIO.writeStdout
  writeStderr = lift . VIO.writeStderr
  writeStdoutLn = lift . VIO.writeStdoutLn
  writeStderrLn = lift . VIO.writeStderrLn

runSilentLoggerT :: SilentLoggerT m a -> m (a, [CompileWarning])
runSilentLoggerT e = runWriterT (unSilentLoggerT e)

--------------------------------------------------------------------------------

runLoggerT ::
  (MonadStdIO m) =>
  LoggingSettings ->
  (forall n. (MonadStdIO n, MonadLogger n) => n a) ->
  m a
runLoggerT loggingSettings@LoggingSettings {..} value = do
  (result, warnings) <-
    if loggingLevel == NoDetail
      then runSilentLoggerT value
      else runLoudLoggerT loggingSettings value

  unless (null warnings) $
    liftIO $
      putLogLn $
        pack $
          showCompileWarnings $
            groupWarnings warnings
  return result

--------------------------------------------------------------------------------
-- Showing warnings

showCompileWarnings :: [SummarisedCompileWarning] -> String
showCompileWarnings ws =
  setTextColour Yellow $
    layoutAsString $
      vsep (fmap (\w -> line <> "Warning: " <+> pretty w <> line) ws)
