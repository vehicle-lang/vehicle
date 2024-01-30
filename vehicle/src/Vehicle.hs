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
import Options.Applicative (ParserInfo, defaultPrefs, execParserPure, handleParseResult)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
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
import Vehicle.Prelude
import Vehicle.Prelude.IO as VIO (MonadStdIO (writeStderrLn))
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
              "No mode provided. Please use one of 'typeCheck', 'compile', 'verify', 'check', 'export'"
          Just mode -> case mode of
            Check options -> typeCheck logSettings options
            Compile options -> compile logSettings options
            Verify options -> verify logSettings options
            Validate options -> validate logSettings options
            Export options -> export logSettings options

withLogger :: (MonadStdIO IO) => GlobalOptions -> (LoggingSettings -> IO a) -> IO a
withLogger GlobalOptions {logFile, loggingLevel} action =
  case logFile of
    Nothing -> action LoggingSettings {putLogLn = VIO.writeStderrLn, loggingLevel}
    Just fp -> do
      createDirectoryIfMissing True (takeDirectory fp)
      withFile fp WriteMode $ \logHandle -> do
        hSetBuffering logHandle NoBuffering
        action LoggingSettings {putLogLn = TextIO.hPutStrLn logHandle, loggingLevel}

execParserWithArgs :: ParserInfo a -> [String] -> IO a
execParserWithArgs parserInfo args =
  handleParseResult (execParserPure defaultPrefs parserInfo args)

handleExitCode :: (MonadIO m) => ExitCode -> m Int
handleExitCode = return . fromExitCode
  where
    fromExitCode :: ExitCode -> Int
    fromExitCode ExitSuccess = 0
    fromExitCode (ExitFailure exitCode) = exitCode
