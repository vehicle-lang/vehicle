{-# LANGUAGE NamedFieldPuns #-}

module Vehicle
  ( main,
    mainWithArgsAndExitCode,
  )
where

import Control.Exception (bracket, handle)
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
      then putStrLn preciseVehicleVersion
      else case modeOptions of
        Nothing ->
          fatalError
            "No mode provided. Please use one of 'typeCheck', 'compile', 'verify', 'check', 'export'"
        Just mode -> case mode of
          Check options -> typeCheck ioSettings options
          Compile options -> compile ioSettings options
          Verify options -> verify ioSettings options
          Validate options -> validate ioSettings options
          Export options -> export ioSettings options

redirections :: GlobalOptions -> (LoggingSettings -> IO a) -> IO a
redirections go = redirectStdout go . redirectStderr go . withLogger go

redirectStdout :: GlobalOptions -> IO a -> IO a
redirectStdout GlobalOptions {outFile} action =
  flip (maybe action) outFile $ \fp -> do
    createDirectoryIfMissing True (takeDirectory fp)
    result <- withFile fp AppendMode $ \fh -> do
      bracket (redirectHandleTo stdout fh) (restoreHandleFrom stdout) (const action)
    return result

redirectStderr :: GlobalOptions -> IO a -> IO a
redirectStderr GlobalOptions {errFile} action =
  flip (maybe action) errFile $ \fp -> do
    createDirectoryIfMissing True (takeDirectory fp)
    withFile fp AppendMode $ \fh -> do
      bracket (redirectHandleTo stderr fh) (restoreHandleFrom stderr) (const action)

withLogger :: GlobalOptions -> (LoggingSettings -> IO a) -> IO a
withLogger GlobalOptions {logFile, loggingLevel} action =
  case logFile of
    Nothing -> action LoggingSettings {logHandle = stderr, loggingLevel}
    Just fp -> do
      createDirectoryIfMissing True (takeDirectory fp)
      withFile fp AppendMode $ \logHandle -> do
        action LoggingSettings {logHandle, loggingLevel}

redirectHandleTo :: Handle -> Handle -> IO Handle
redirectHandleTo source target = do
  backup <- hDuplicate source
  hDuplicateTo target source
  return backup

restoreHandleFrom :: Handle -> Handle -> IO ()
restoreHandleFrom source backup = do
  hDuplicateTo backup source

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
