module Vehicle
  ( run
  , Options(..)
  , GlobalOptions(..)
  , defaultGlobalOptions
  , ModeOptions(..)
  ) where

import Control.Exception (bracket)
import Control.Monad (when,)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO

import Vehicle.Prelude
import Vehicle.Compile (CompileOptions(..), compile)
import Vehicle.Check (CheckOptions(..), check)
import Vehicle.Verify (VerifyOptions(..), verify)
import Vehicle.Export (ExportOptions, export)

--------------------------------------------------------------------------------
-- Main command

data Options = Options
  { globalOptions :: GlobalOptions
  , modeOptions   :: Maybe ModeOptions
  } deriving (Eq, Show)

data GlobalOptions = GlobalOptions
  { version       :: Bool
  , outFile       :: Maybe FilePath
  , errFile       :: Maybe FilePath
  , logFile       :: Maybe FilePath
  , loggingLevel  :: LoggingLevel
  } deriving (Eq, Show)

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions = GlobalOptions
  { version      = False
  , outFile      = Nothing
  , errFile      = Nothing
  , logFile      = Nothing
  , loggingLevel = defaultLoggingLevel
  }

data ModeOptions
  = Compile CompileOptions
  | Verify  VerifyOptions
  | Check   CheckOptions
  | Export  ExportOptions
   deriving (Eq, Show)

run :: Options -> IO ()
run Options{..} = do
  let GlobalOptions{..} = globalOptions
  when version $ do
    print vehicleVersion
    exitSuccess

  let acquireOutputHandles = openHandles  (outFile, errFile, logFile, loggingLevel)
  let releaseOutputHandles = closeHandles (outFile, errFile, logFile)

  bracket acquireOutputHandles releaseOutputHandles $ \ioSettings ->
    case modeOptions of
      Nothing   -> outputErrorAndQuit ioSettings "No mode provided. Please use one of 'compile', 'verify', 'check', 'export'"
      Just mode -> case mode of
        Compile options -> compile ioSettings options
        Verify  options -> verify  ioSettings options
        Check   options -> check   ioSettings options
        Export  options -> export  ioSettings options

openHandles :: (Maybe FilePath, Maybe FilePath, Maybe FilePath, LoggingLevel)
            -> IO VehicleIOSettings
openHandles (outFile, errFile, logFile, logLevel) = do
  outputHandle <- case outFile of
    Nothing   -> return stdout
    Just file -> openHandle file

  errorHandle <- case errFile of
    Nothing   -> return stderr
    Just file -> openHandle file

  logHandle <- case logFile of
    Nothing   -> return stdout
    Just file -> openHandle file

  return VehicleIOSettings
    { errorHandle  = errorHandle
    , outputHandle = outputHandle
    , logHandle    = logHandle
    , loggingLevel = logLevel
    }

closeHandles :: (Maybe FilePath, Maybe FilePath, Maybe FilePath)
             -> VehicleIOSettings
             -> IO ()
closeHandles (outFile, errFile, logFile) VehicleIOSettings{..} = do
  case outFile of
    Nothing -> return ()
    Just _  -> hClose outputHandle

  case errFile of
    Nothing -> return ()
    Just _  -> hClose errorHandle

  case logFile of
    Nothing  -> return ()
    Just _   -> hClose logHandle

openHandle :: FilePath -> IO Handle
openHandle file = do
  exists <- doesFileExist file
  if exists
    then openFile file AppendMode
    else do
      -- Must be a better way of doing this...
      createEmptyFile file
      openFile file WriteMode

createEmptyFile :: FilePath -> IO ()
createEmptyFile path = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path ""