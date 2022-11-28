module Vehicle.Prelude.IO
<<<<<<< HEAD
  ( removeFileIfExists
=======
  ( vehicleFileExtension
  , vehicleInterfaceFileExtension
  , vehicleProofCacheFileExtension
  , VehicleIOSettings(..)
  , fromLoggedIO
  , fromLoggerTIO
  , outputErrorAndQuit
  , removeFileIfExists
>>>>>>> Add interface files to cache type-checking
  , fatalError
  , programOutput
  ) where

import Control.Exception (catch, throwIO)
-- import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Prettyprinter (Doc)
import System.Directory (removeFile)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import System.IO.Error (isDoesNotExistError)

<<<<<<< HEAD
{-
fromLoggerTIO :: LoggingSettings -> ImmediateLogger a -> IO a
fromLoggerTIO options@LoggingSettings{..} logger = do
  (v, messages) <- runLogger loggingLevel logger
=======
vehicleFileExtension :: String
vehicleFileExtension = ".vcl"

vehicleInterfaceFileExtension :: String
vehicleInterfaceFileExtension = vehicleFileExtension <> "i"

vehicleProofCacheFileExtension :: String
vehicleProofCacheFileExtension = vehicleFileExtension <> "p"

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
>>>>>>> Add interface files to cache type-checking
  fromLoggedIO options $ do
    forM_ messages logMessage
    return v

flushLogger :: MonadIO m => LoggingLevel -> Handle -> Logger a -> m a
flushLogger debugLevel logHandle logger = do
  (v, messages) <- liftIO $ runLogger debugLevel logger
  flushLogs logHandle messages
  return v

flushLogs :: MonadIO m => Handle -> [Message] -> m ()
flushLogs logHandle messages = liftIO $ mapM_ (hPrint logHandle) messages
-}

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
