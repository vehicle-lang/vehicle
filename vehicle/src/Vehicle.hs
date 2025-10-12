{-# LANGUAGE NamedFieldPuns #-}

module Vehicle
  ( mainWithArgsAndExitCode,
  )
where

import Control.Exception (Exception (..), Handler (..), SomeException (..), catches, handle, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as Text (pack)
import Data.Text.IO qualified as TextIO (hPutStrLn)
import GHC.IO.Encoding (setLocaleEncoding)
import Options.Applicative (ParserInfo, ParserResult (CompletionInvoked, Failure, Success), defaultPrefs, execCompletion, execParserPure, renderFailure)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getProgName)
import System.Exit (ExitCode (..), exitFailure, exitSuccess, exitWith)
import System.FilePath (takeDirectory)
import System.IO
  ( BufferMode (NoBuffering),
    IOMode (WriteMode),
    hSetBuffering,
    utf8,
    withFile,
  )
import Vehicle.CommandLine (GlobalOptions (..), ModeOptions (..), Options (..), commandLineOptionsParserInfo)
import Vehicle.Compile (compile)
import Vehicle.Export (export)
import Vehicle.List (list)
import Vehicle.Prelude
import Vehicle.Prelude.IO as VIO (MonadStdIO (writeStderrLn), fatalError, programOutput)
import Vehicle.Prelude.Logging
import Vehicle.TypeCheck (typeCheck)
import Vehicle.Validate (validate)
import Vehicle.Verify (verify)

--------------------------------------------------------------------------------
-- Main command

mainWithArgsAndExitCode :: (MonadStdIO IO) => [String] -> IO Int
mainWithArgsAndExitCode args = do
  setLocaleEncoding utf8
  -- Catch any exits and return the exit code, which is important when using
  -- the main function from a library, because exits are uncaught exceptions.
  handle handleExitCode $ do
    options <- execParserWithArgs commandLineOptionsParserInfo args
    runVehicle options
    liftIO exitSuccess

rethrowExitCode :: ExitCode -> IO ()
rethrowExitCode = throwIO

uncaughtException :: (MonadStdIO IO) => SomeException -> IO ()
uncaughtException (SomeException e) = do
  writeStderrLn (Text.pack $ displayException e)
  exitFailure

runVehicle :: (MonadStdIO IO) => Options -> IO ()
runVehicle Options {..} = do
  withLogger globalOptions $ \logSettings -> do
    -- Catch uncaught exceptions
    flip catches [Handler rethrowExitCode, Handler uncaughtException] $ do
      -- Handle --version
      if version globalOptions
        then writeStdoutLn (Text.pack preciseVehicleVersion)
        else case modeOptions of
          Nothing ->
            fatalError
              "No mode provided. Please use one of 'check', 'compile','verify', 'check', 'export', 'list'"
          Just mode -> case mode of
            Check options -> typeCheck logSettings outputAsJson options
            Compile options -> compile logSettings outputAsJson options
            Verify options -> verify logSettings outputAsJson options
            Validate options -> validate logSettings outputAsJson options
            Export options -> export logSettings outputAsJson options
            List options -> list logSettings outputAsJson options
            where
              outputAsJson = outputAsJSON globalOptions

withLogger :: (MonadStdIO IO) => GlobalOptions -> (LoggingSettings -> IO a) -> IO a
withLogger GlobalOptions {logFile, loggingPass, loggingLevel, noWarnings} action = do
  let runAction logLn = action LoggingSettings {putLogLn = logLn, loggingPass, loggingLevel, noWarnings}
  case logFile of
    Nothing -> runAction VIO.writeStderrLn
    Just fp -> do
      createDirectoryIfMissing True (takeDirectory fp)
      withFile fp WriteMode $ \logHandle -> do
        hSetBuffering logHandle NoBuffering
        runAction (TextIO.hPutStrLn logHandle)

execParserWithArgs :: (MonadStdIO m) => ParserInfo a -> [String] -> m a
execParserWithArgs parserInfo args =
  handleParseResult (execParserPure defaultPrefs parserInfo args)

handleExitCode :: (MonadIO m) => ExitCode -> m Int
handleExitCode = return . fromExitCode
  where
    fromExitCode :: ExitCode -> Int
    fromExitCode ExitSuccess = 0
    fromExitCode (ExitFailure exitCode) = exitCode

-- Inlining Options.Applicative handleParserResult to enable stdout and stderr to be piped
handleParseResult :: (MonadStdIO m) => ParserResult a -> m a
handleParseResult (Success a) = return a
handleParseResult (Failure failure) = do
  progn <- liftIO getProgName
  let (msg, exit) = renderFailure failure progn
  case exit of
    ExitSuccess -> VIO.programOutput (pretty msg)
    _ -> VIO.fatalError (pretty msg)
  liftIO $ exitWith exit
handleParseResult (CompletionInvoked compl) = do
  progn <- liftIO getProgName
  msg <- liftIO $ execCompletion compl progn
  VIO.programOutput (pretty msg)
  liftIO exitSuccess
