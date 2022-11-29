module Vehicle.Prelude.IO
  ( removeFileIfExists
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

{-
fromLoggerTIO :: LoggingSettings -> ImmediateLogger a -> IO a
fromLoggerTIO options@LoggingSettings{..} logger = do
  (v, messages) <- runLogger loggingLevel logger
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
