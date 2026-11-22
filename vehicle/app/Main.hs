{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import Vehicle (mainWithArgsAndExitCode)
import Vehicle.Prelude (MonadStdIO (..))

instance MonadStdIO IO where
  {-# INLINE writeStdout #-}
  writeStdout :: Text -> IO ()
  writeStdout = TextIO.putStr
  {-# INLINE writeStderr #-}
  writeStderr :: Text -> IO ()
  writeStderr = TextIO.hPutStr stderr
  {-# INLINE writeStdoutLn #-}
  writeStdoutLn :: Text -> IO ()
  writeStdoutLn = TextIO.putStrLn
  {-# INLINE writeStderrLn #-}
  writeStderrLn :: Text -> IO ()
  writeStderrLn = TextIO.hPutStrLn stderr

main :: IO ()
main = do
  args <- getArgs
  exitCode <- mainWithArgsAndExitCode args
  exitWith (toExitCode exitCode)
  where
    toExitCode :: Int -> ExitCode
    toExitCode exitCode
      | exitCode == 0 = ExitSuccess
      | otherwise = ExitFailure exitCode
