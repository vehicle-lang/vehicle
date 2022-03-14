module Vehicle
  ( run
  , Options(..)
  , Command(..)
  ) where

import Control.Exception (bracket)
import Control.Monad (when,)
import System.Exit (exitSuccess)
import System.IO

import Vehicle.Prelude
import Vehicle.Compile (CompileOptions(..), compile)
import Vehicle.Check (CheckOptions(..), check)
import Vehicle.Verify (VerifyOptions(..), verify)

--------------------------------------------------------------------------------
-- Main command

run :: Options -> IO ()
run Options{..} = do
  when version $ do
    print vehicleVersion
    exitSuccess

  let acquireOutputHandles = openHandles (errFile, logFile)
  let releaseOutputHandles = closeHandles (errFile, logFile)

  bracket acquireOutputHandles releaseOutputHandles $ \loggingSettings ->
    case commandOption of
      Compile options -> compile loggingSettings options
      Verify  options -> verify  loggingSettings options
      Check   options -> check   loggingSettings options


openHandles :: (Maybe FilePath, Maybe (Maybe FilePath))
            -> IO LoggingOptions
openHandles (errFile, logFile) = do
  errorHandle <- case errFile of
    Nothing -> return stderr
    Just x  -> openFile x AppendMode

  let outputHandle = stdout

  logHandle <- case logFile of
    Nothing       -> return Nothing
    Just Nothing  -> return (Just stdout)
    Just (Just x) -> Just <$> openFile x AppendMode

  return LoggingOptions
    { errorHandle  = errorHandle
    , outputHandle = outputHandle
    , logHandle    = logHandle
    }

closeHandles :: (Maybe FilePath, Maybe (Maybe FilePath))
             -> LoggingOptions
             -> IO ()
closeHandles (errFile, logFile) LoggingOptions{..} = do
  case errFile of
    Nothing -> return ()
    Just _  -> hClose errorHandle

  case (logFile, logHandle) of
    (Just (Just _), Just logH) -> hClose logH
    _                          -> return ()


data Options = Options
  { version       :: Bool
  , logFile       :: Maybe (Maybe FilePath)
  , errFile       :: Maybe FilePath
  , commandOption :: Command
  } deriving (Show)

data Command
  = Compile CompileOptions
  | Verify  VerifyOptions
  | Check   CheckOptions
   deriving (Show)