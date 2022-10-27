module Vehicle.Prelude.IO
  ( VehicleIOSettings(..)
  , fromLoggedIO
  , fromLoggerTIO
  , outputErrorAndQuit
  ) where

import Control.Monad (forM_)
import Control.Monad.Trans (MonadIO (..))
import Data.Text.IO as T (hPutStrLn)
import System.IO

import System.Exit (exitFailure)
import Vehicle.Prelude.Logging
import Vehicle.Prelude.Prettyprinter

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
