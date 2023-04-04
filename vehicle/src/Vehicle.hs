{-# LANGUAGE NamedFieldPuns #-}

module Vehicle
  ( main,
    mainWithArgsAndExitCode,
  )
where

import Control.Exception (bracket, finally, handle)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import GHC.IO.Encoding (setLocaleEncoding)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Options.Applicative (ParserInfo, defaultPrefs, execParserPure, handleParseResult)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeDirectory)
import System.IO
import Vehicle.CommandLine (GlobalOptions (..), ModeOptions (..), Options (..), commandLineOptionsParserInfo)
import Vehicle.Compile (compile)
import Vehicle.CompileAndVerify (compileAndVerify)
import Vehicle.Export (export)
import Vehicle.Prelude
import Vehicle.TypeCheck (typeCheck)
import Vehicle.Validate (validate)
import Vehicle.Verify (verify)

--------------------------------------------------------------------------------
-- Main command

main :: IO ()
main = do
  args <- getArgs
  exitCode <- mainWithArgsAndExitCode args
  exitWith (toExitCode exitCode)

mainWithArgsAndExitCode :: [String] -> IO Int
mainWithArgsAndExitCode args = do
  setLocaleEncoding utf8
  -- Catch any exits and return the exit code, which is important when using
  -- the main function from a library, because exits are uncaught exceptions.
  handle handleExitCode $ do
    options <- execParserWithArgs commandLineOptionsParserInfo args
    runVehicle options
    exitSuccess

runVehicle :: Options -> IO ()
runVehicle Options {..} = do
  redirections globalOptions $ \ioSettings -> do
    -- Handle --version
    if version globalOptions
      then putStrLn $ showVersion vehicleVersion
      else case modeOptions of
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

redirections :: GlobalOptions -> (LoggingSettings -> IO a) -> IO a
redirections globalOptions action = do
  redirectOutput globalOptions $
    redirectError globalOptions $
      withLogger globalOptions action

redirectOutput :: GlobalOptions -> IO a -> IO a
redirectOutput GlobalOptions {outFile} action =
  flip (maybe action) outFile $ \fp -> do
    createDirectoryIfMissing True (takeDirectory fp)
    withFile fp AppendMode $ \fh -> do
      bracket (redirectTo stdout fh) (restore stdout) (const action)

redirectError :: GlobalOptions -> IO a -> IO a
redirectError GlobalOptions {errFile} action =
  flip (maybe action) errFile $ \fp -> do
    createDirectoryIfMissing True (takeDirectory fp)
    withFile fp AppendMode $ \fh -> do
      bracket (redirectTo stderr fh) (restore stderr) (const action)

withLogger :: GlobalOptions -> (LoggingSettings -> IO a) -> IO a
withLogger GlobalOptions {logFile, loggingLevel} action =
  case logFile of
    Nothing -> action LoggingSettings {logHandle = stderr, loggingLevel}
    Just fp -> do
      createDirectoryIfMissing True (takeDirectory fp)
      withFile fp AppendMode $ \logHandle -> do
        action LoggingSettings {logHandle, loggingLevel}

redirectTo :: Handle -> Handle -> IO Handle
redirectTo source target = do
  backup <- hDuplicate source
  hDuplicateTo source target
  return backup

restore :: Handle -> Handle -> IO ()
restore source backup = do
  hDuplicateTo source backup

toExitCode :: Int -> ExitCode
toExitCode exitCode
  | exitCode == 0 = ExitSuccess
  | otherwise = ExitFailure exitCode

execParserWithArgs :: ParserInfo a -> [String] -> IO a
execParserWithArgs parserInfo args =
  handleParseResult (execParserPure defaultPrefs parserInfo args)

handleExitCode :: ExitCode -> IO Int
handleExitCode = return . fromExitCode
  where
    fromExitCode :: ExitCode -> Int
    fromExitCode ExitSuccess = 0
    fromExitCode (ExitFailure exitCode) = exitCode
