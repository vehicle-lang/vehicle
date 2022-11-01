module Vehicle.Prelude.IO
  ( VehicleIOSettings(..)
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
import System.Directory (removeFile)
import System.Exit (exitFailure)
import System.IO (Handle, hPrint)
import System.IO.Error (isDoesNotExistError)
import Vehicle.Prelude.Logging (LoggerT, LoggingLevel, Message,
                                MonadLogger (logMessage), runLoggerT)
import Vehicle.Prelude.Prettyprinter (Doc, layoutAsText)

data VehicleIOSettings = VehicleIOSettings
  { errorHandle  :: Handle
  , outputHandle :: Handle
  , logHandle    :: Handle
  , loggingLevel :: LoggingLevel
  }

fromLoggedIO :: MonadIO m => VehicleIOSettings -> LoggerT m a -> m a
fromLoggedIO VehicleIOSettings{..} = flushLogger loggingLevel logHandle

fromLoggerTIO :: VehicleIOSettings -> LoggerT IO a -> IO a
fromLoggerTIO options@VehicleIOSettings{..} logger = do
  (v, messages) <- runLoggerT loggingLevel logger
  fromLoggedIO options $ do
    forM_ messages logMessage
    return v

outputErrorAndQuit :: VehicleIOSettings -> Doc b -> IO a
outputErrorAndQuit VehicleIOSettings{..} message= do
  T.hPutStrLn errorHandle $ layoutAsText message
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

fatalError :: MonadIO m => VehicleIOSettings -> Doc a -> m b
fatalError VehicleIOSettings{..} message = liftIO $ do
  hPrint errorHandle message
  exitFailure

programOutput :: MonadIO m => VehicleIOSettings -> Doc a -> m ()
programOutput VehicleIOSettings{..} message = liftIO $ hPrint outputHandle message
