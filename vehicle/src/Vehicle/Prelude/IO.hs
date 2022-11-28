module Vehicle.Prelude.IO
  ( LoggingSettings(..)
  , fromLoggedIO
  , fromLoggerTIO
  , outputErrorAndQuit
  , removeFileIfExists
  , fatalError
  , programOutput
  ) where

import Control.Exception (catch, throwIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text.IO as T (hPutStrLn)
import Prettyprinter (Doc)
import System.Directory (removeFile)
import System.Exit (exitFailure)
import System.IO (Handle, hPrint, stderr)
import System.IO.Error (isDoesNotExistError)

import Vehicle.Prelude.Logging (LoggerT, LoggingLevel, Message,
                                MonadLogger (logMessage), runLoggerT)
import Vehicle.Syntax.Prelude (layoutAsText)

data LoggingSettings = LoggingSettings
  { logHandle    :: Handle
  , loggingLevel :: LoggingLevel
  }

fromLoggedIO :: MonadIO m => LoggingSettings -> LoggerT m a -> m a
fromLoggedIO LoggingSettings{..} = flushLogger loggingLevel logHandle

fromLoggerTIO :: LoggingSettings -> LoggerT IO a -> IO a
fromLoggerTIO options@LoggingSettings{..} logger = do
  (v, messages) <- runLoggerT loggingLevel logger
  fromLoggedIO options $ do
    forM_ messages logMessage
    return v

outputErrorAndQuit :: Doc b -> IO a
outputErrorAndQuit message= do
  T.hPutStrLn stderr $ layoutAsText message
  exitFailure

flushLogger :: MonadIO m => LoggingLevel -> Handle -> LoggerT m a -> m a
flushLogger debugLevel logHandle logger = do
  (v, messages) <- runLoggerT debugLevel logger
  flushLogs logHandle messages
  return v

flushLogs :: MonadIO m => Handle -> [Message] -> m ()
flushLogs logHandle messages = liftIO $ mapM_ (hPrint logHandle) messages


--------------------------------------------------------------------------------
-- IO operations

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

fatalError :: MonadIO m => Doc a -> m b
fatalError message = liftIO $ do
  hPrint stderr message
  exitFailure

programOutput :: MonadIO m => Doc a -> m ()
programOutput message = liftIO $ print message
