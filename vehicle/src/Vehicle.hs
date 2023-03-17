module Vehicle
  ( run,
    Options (..),
    GlobalOptions (..),
    defaultGlobalOptions,
    ModeOptions (..),
  )
where

import Control.Exception (bracket)
import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitSuccess)
import System.FilePath (takeDirectory)
import System.IO
import Vehicle.Compile (CompileOptions (..), compile)
import Vehicle.CompileAndVerify (CompileAndVerifyOptions (..), compileAndVerify)
import Vehicle.Export (ExportOptions, export)
import Vehicle.Prelude
import Vehicle.TypeCheck (TypeCheckOptions, typeCheck)
import Vehicle.Validate (ValidateOptions (..), validate)
import Vehicle.Verify (VerifyOptions (..), verify)

--------------------------------------------------------------------------------
-- Main command

data Options = Options
  { globalOptions :: GlobalOptions,
    modeOptions :: Maybe ModeOptions
  }
  deriving (Eq, Show)

data GlobalOptions = GlobalOptions
  { version :: Bool,
    logFile :: Maybe FilePath,
    loggingLevel :: LoggingLevel
  }
  deriving (Eq, Show)

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { version = False,
      logFile = Nothing,
      loggingLevel = defaultLoggingLevel
    }

data ModeOptions
  = Check TypeCheckOptions
  | Compile CompileOptions
  | Verify VerifyOptions
  | CompileAndVerify CompileAndVerifyOptions
  | Validate ValidateOptions
  | Export ExportOptions
  deriving (Eq, Show)

run :: Options -> IO ()
run Options {..} = do
  let GlobalOptions {..} = globalOptions
  when version $ do
    print vehicleVersion
    exitSuccess

  let acquireHandles = createLoggingSettings (logFile, loggingLevel)
  let releaseHandles = destroyLoggingSettings logFile

  bracket acquireHandles releaseHandles $ \ioSettings ->
    case modeOptions of
      Nothing ->
        fatalError
          "No mode provided. Please use one of 'typeCheck', 'compile', 'verify', 'check', 'export'"
      Just mode -> case mode of
        Check options -> typeCheck ioSettings options
        Compile options -> compile ioSettings options
        Verify options -> verify ioSettings options
        CompileAndVerify options -> compileAndVerify ioSettings options
        Validate options -> validate ioSettings options
        Export options -> export ioSettings options

createLoggingSettings ::
  (Maybe FilePath, LoggingLevel) ->
  IO LoggingSettings
createLoggingSettings (logFile, logLevel) = do
  logHandle <- case logFile of
    Nothing -> return stdout
    Just file -> openHandle file

  return
    LoggingSettings
      { logHandle = logHandle,
        loggingLevel = logLevel
      }

destroyLoggingSettings ::
  Maybe FilePath ->
  LoggingSettings ->
  IO ()
destroyLoggingSettings logFile LoggingSettings {..} = do
  case logFile of
    Nothing -> return ()
    Just _ -> hClose logHandle

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
