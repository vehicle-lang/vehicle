{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Vehicle.Prelude.Logging.Instance
  ( LoggerT (..),
    LoggingSettings (..),
    runLoggerT,
    runSilentLoggerT,
    showCompileWarnings,
    OutputAsJSON,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text, pack, unpack)
import System.Console.ANSI (Color (..))
import Vehicle.Compile.Print.Warning ()
import Vehicle.Prelude
import Vehicle.Prelude.IO as VIO (MonadStdIO (..))
import Vehicle.Prelude.Logging.Class
import Vehicle.Prelude.Warning
import Vehicle.Verify.Specification.IO.Read (MonadReadSpecification, readSpecification)

--------------------------------------------------------------------------------
-- Settings

type OutputAsJSON = Bool

data LoggingSettings = LoggingSettings
  { putLogLn :: Text -> IO (),
    loggingLevel :: LoggingLevel,
    loggingTarget :: CompilerStack,
    noWarnings :: Bool
  }

--------------------------------------------------------------------------------
-- Standard logging monad

data LoggingState = LoggingState
  { currentStack :: !CompilerStack,
    currentDepth :: !Int,
    matchesTargetStack :: !Bool
  }

initialLoggingState :: LoggingSettings -> LoggingState
initialLoggingState LoggingSettings {..} =
  LoggingState
    { currentStack = emptyStack,
      currentDepth = 0,
      matchesTargetStack = stackMatches emptyStack loggingTarget
    }

updateLoggingState ::
  LoggingSettings ->
  Either CompilerPass Identifier ->
  LoggingState ->
  LoggingState
updateLoggingState LoggingSettings {..} item oldState = do
  let newStack = consStack (second (unpack . nameOf) item) (currentStack oldState)
  let newState =
        oldState
          { currentStack = newStack,
            matchesTargetStack = stackMatches newStack loggingTarget
          }
  newState

-- | The read-only environment of the logger. The mutable parts of the logging
-- state are stored in `IORef`s so that the monad is a simple `ReaderT` over
-- the underlying monad.
data LoggerEnv = LoggerEnv
  { loggerSettings :: !LoggingSettings,
    -- | Cached so that checking whether logging is enabled is a single field read.
    loggerSilent :: !Bool,
    loggerState :: !(IORef LoggingState),
    loggerWarnings :: !(IORef [CompileWarning])
  }

newtype LoggerT m a = LoggerT
  { unLoggerT :: ReaderT LoggerEnv m a
  }
  deriving newtype (Functor, Applicative, Monad)

-- | Only runs the action if logging is enabled.
unlessSilent :: (Monad m) => LoggerT m () -> LoggerT m ()
unlessSilent action = do
  silent <- LoggerT $ asks loggerSilent
  unless silent action
{-# INLINE unlessSilent #-}

readLoggingState :: (MonadIO m) => LoggerT m LoggingState
readLoggingState = LoggerT $ do
  ref <- asks loggerState
  liftIO $ readIORef ref
{-# INLINE readLoggingState #-}

writeLoggingState :: (MonadIO m) => LoggingState -> LoggerT m ()
writeLoggingState newState = LoggerT $ do
  ref <- asks loggerState
  liftIO $ writeIORef ref newState
{-# INLINE writeLoggingState #-}

modifyLoggingState :: (MonadIO m) => (LoggingState -> LoggingState) -> LoggerT m ()
modifyLoggingState f = LoggerT $ do
  ref <- asks loggerState
  liftIO $ modifyIORef' ref f
{-# INLINE modifyLoggingState #-}

-- | Updates the compiler stack for the duration of the action. When logging
-- is disabled the stack is irrelevant and so this is just the action.
withStackEntry :: (MonadIO m) => Either CompilerPass Identifier -> LoggerT m a -> LoggerT m a
withStackEntry item action = do
  silent <- LoggerT $ asks loggerSilent
  if silent
    then action
    else do
      settings <- LoggerT $ asks loggerSettings
      oldState <- readLoggingState
      writeLoggingState $ updateLoggingState settings item oldState
      result <- action
      writeLoggingState oldState
      return result

instance (MonadIO m) => MonadLogger (LoggerT m) where
  setCallDepth x =
    unlessSilent $ modifyLoggingState (\s -> s {currentDepth = x})

  getCallDepth = currentDepth <$> readLoggingState

  incrCallDepth =
    unlessSilent $ modifyLoggingState (\s -> s {currentDepth = currentDepth s + 1})

  decrCallDepth =
    unlessSilent $ modifyLoggingState (\s -> s {currentDepth = currentDepth s - 1})

  getDebugLevel = LoggerT $ asks (loggingLevel . loggerSettings)

  logWarning w = LoggerT $ do
    ref <- asks loggerWarnings
    liftIO $ modifyIORef' ref (w :)

  logMessage message =
    unlessSilent $ do
      loggingEnabled <- matchesTargetStack <$> readLoggingState
      when loggingEnabled $ LoggerT $ do
        logAction <- asks (putLogLn . loggerSettings)
        liftIO $ logAction (pack $ show message)

  runCompilerPass compilerPass = withStackEntry (Left compilerPass)

  runCompileDecl decl = withStackEntry (Right decl)

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

instance (MonadReadSpecification m) => MonadReadSpecification (LoggerT m) where
  readSpecification = lift . readSpecification

--------------------------------------------------------------------------------
-- Running the logger

runLoggerTWithWarnings ::
  (MonadIO m) =>
  LoggingSettings ->
  LoggerT m a ->
  m (a, [CompileWarning])
runLoggerTWithWarnings loggingSettings (LoggerT value) = do
  stateRef <- liftIO $ newIORef (initialLoggingState loggingSettings)
  warningsRef <- liftIO $ newIORef []
  let env =
        LoggerEnv
          { loggerSettings = loggingSettings,
            loggerSilent = loggingLevel loggingSettings == NoDetail,
            loggerState = stateRef,
            loggerWarnings = warningsRef
          }
  result <- runReaderT value env
  warnings <- liftIO $ readIORef warningsRef
  return (result, reverse warnings)

-- | Runs the logger without logging, returning any warnings generated.
runSilentLoggerT :: (MonadIO m) => LoggerT m a -> m (a, [CompileWarning])
runSilentLoggerT =
  runLoggerTWithWarnings $
    LoggingSettings
      { putLogLn = const (return ()),
        loggingLevel = NoDetail,
        loggingTarget = emptyStack,
        noWarnings = True
      }

runLoggerT ::
  (MonadIO m) =>
  LoggingSettings ->
  LoggerT m a ->
  m a
runLoggerT loggingSettings@LoggingSettings {..} value = do
  (result, warnings) <- runLoggerTWithWarnings loggingSettings value
  unless (null warnings || noWarnings) $
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
      vsep (fmap (\w -> line <> "Warning:" <+> pretty w <> line) ws)
