#!/usr/bin/env cabal
{- cabal:
build-depends:
  , base       >=4   && <5
  , containers >=0.5 && <0.6
  , directory  >=1   && <2
  , filepath   >=1   && <2
  , process    >=1.2 && <2

default-language:   Haskell2010
ghc-options:        -Wall
-}
{-# LANGUAGE LambdaCase #-}
import Control.Monad (filterM, forM, unless)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (isRelative)
import System.IO (hPutStrLn, stderr)
import System.Process (spawnProcess, waitForProcess)

main :: IO ()
main = do
  -- Find 'hie-bios':
  hieBios <- findHieBios

  -- Get all arguments:
  files <- getArgs

  -- Check that all the file arguments exist:
  notExist <- filterM (fmap not . doesFileExist) files
  unless (null notExist) $ fail $ unlines $
    "Error: vehicle-hie-bios-check was called with non-existent file paths:"
      : ["- " <> file | file <- notExist]

  -- Check that all file paths are relative:
  let notRelative = filter (not . isRelative) files
  unless (null notRelative) $ fail $ unlines $
    "Error: vehicle-hie-bios-check was called with non-relative file paths:"
      : ["- " <> file | file <- notExist]

  exitCodes <-
    forM files $ \file -> do
      putStrLn $ "Checking " <> file
      processHandle <- spawnProcess hieBios ["check", file]
      waitForProcess processHandle

  -- Exit with success only if all calls to stylish-haskell succeeded
  exitWith $ if all isExitSuccess exitCodes then ExitSuccess else ExitFailure 1

isExitSuccess :: ExitCode -> Bool
isExitSuccess ExitSuccess = True
isExitSuccess _           = False

findHieBios :: IO FilePath
findHieBios = do
  findExecutable "hie-bios" >>= \case
    Just stylishHaskell -> return stylishHaskell
    Nothing             -> do
      hPutStrLn stderr
        "Error: The pre-commit hook requires 'hie-bios' to format Haskell code.\
        \       You can install 'hie-bios' by running:\
        \\
        \       cabal v2-install hie-bios --ignore-project --overwrite-policy=always\
        \\
        \       See: https://github.com/haskell/hie-bios#readme"
      exitFailure
