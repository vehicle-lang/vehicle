module Vehicle
  ( run
  , Options(..)
  , ModeOptions(..)
  ) where

import Control.Exception (bracket)
import Control.Monad (when,)
import System.Exit (exitSuccess)
import System.IO

import Vehicle.Prelude
import Vehicle.Compile (CompileOptions(..), compile)
import Vehicle.Check (CheckOptions(..), check)
import Vehicle.Verify (VerifyOptions(..), verify)
import Vehicle.Export (ExportOptions, export)

--------------------------------------------------------------------------------
-- Main command

data Options = Options
  { version       :: Bool
  , outFile       :: Maybe FilePath
  , errFile       :: Maybe FilePath
  , logFile       :: Maybe (Maybe FilePath)
  , debugLevel    :: Int
  , modeOptions   :: ModeOptions
  } deriving (Show)

data ModeOptions
  = Compile CompileOptions
  | Verify  VerifyOptions
  | Check   CheckOptions
  | Export  ExportOptions
   deriving (Show)

run :: Options -> IO ()
run Options{..} = do
  when version $ do
    print vehicleVersion
    exitSuccess

  let acquireOutputHandles = openHandles  (outFile, errFile, logFile, debugLevel)
  let releaseOutputHandles = closeHandles (outFile, errFile, logFile)

  bracket acquireOutputHandles releaseOutputHandles $ \loggingSettings ->
    case modeOptions of
      Compile options -> compile loggingSettings options
      Verify  options -> verify  loggingSettings options
      Check   options -> check   loggingSettings options
      Export  options -> export  loggingSettings options

openHandles :: (Maybe FilePath, Maybe FilePath, Maybe (Maybe FilePath), Int)
            -> IO LoggingOptions
openHandles (outFile, errFile, logFile, logLevel) = do
  outputHandle <- case outFile of
    Nothing -> return stdout
    Just x  -> openFile x AppendMode

  errorHandle <- case errFile of
    Nothing -> return stderr
    Just x  -> openFile x AppendMode

  logHandle <- case logFile of
    Nothing       -> return Nothing
    Just Nothing  -> return (Just stdout)
    Just (Just x) -> Just <$> openFile x AppendMode

  let debugLevel = case logLevel of
        l | l <= 1    -> MinDetail
          | l == 2    -> MidDetail
          | otherwise -> MaxDetail

  return LoggingOptions
    { errorHandle  = errorHandle
    , outputHandle = outputHandle
    , logHandle    = logHandle
    , debugLevel   = debugLevel
    }

closeHandles :: (Maybe FilePath, Maybe FilePath, Maybe (Maybe FilePath))
             -> LoggingOptions
             -> IO ()
closeHandles (outFile, errFile, logFile) LoggingOptions{..} = do
  case outFile of
    Nothing -> return ()
    Just _  -> hClose outputHandle

  case errFile of
    Nothing -> return ()
    Just _  -> hClose errorHandle

  case (logFile, logHandle) of
    (Just (Just _), Just logH) -> hClose logH
    _                          -> return ()
