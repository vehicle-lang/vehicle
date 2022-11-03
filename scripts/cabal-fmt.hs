#!/usr/bin/env cabal
{- cabal:
build-depends:
    base       >=4   && <5
  , directory  >=1   && <2
  , filepath   >=1   && <2
  , process    >=1.2 && <2

default-language:   Haskell2010
ghc-options:        -Wall
-}
{-# LANGUAGE LambdaCase #-}
import Control.Monad (filterM, forM, unless)
import Data.List (isPrefixOf, partition)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (isRelative)
import System.IO (hPutStrLn, stderr)
import System.Process (spawnProcess, waitForProcess)

main :: IO ()
main = do
  -- Find 'cabal-fmt':
  cabalFmt <- findCabalFmt

  -- Separate out flags from files:
  (flags, files) <- partition ("-" `isPrefixOf`) <$> getArgs

  -- Check that all the file arguments exist:
  notExist <- filterM (fmap not . doesFileExist) files
  unless (null notExist) $ fail $ unlines $
    "Error: vehicle-cabal-fmt was called with non-existent file paths:"
      : ["- " <> file | file <- notExist]

  -- Check that all file paths are relative:
  let notRelative = filter (not . isRelative) files
  unless (null notRelative) $ fail $ unlines $
    "Error: vehicle-cabal-fmt was called with non-relative file paths:"
      : ["- " <> file | file <- notExist]

  -- Call stylish-haskell for each project root:
  exitCodes <-
    forM files $ \file -> do
      putStrLn $ "Checking " <> file
      processHandle <- spawnProcess cabalFmt (flags <> [file])
      waitForProcess processHandle

  -- Exit with success only if all calls to stylish-haskell succeeded
  exitWith $ if all isExitSuccess exitCodes then ExitSuccess else ExitFailure 1

isExitSuccess :: ExitCode -> Bool
isExitSuccess ExitSuccess = True
isExitSuccess _           = False

findCabalFmt :: IO FilePath
findCabalFmt = do
  findExecutable "cabal-fmt" >>= \case
    Just stylishHaskell -> return stylishHaskell
    Nothing             -> do
      hPutStrLn stderr . unlines $
        [ "The pre-commit hook requires 'cabal-fmt' to format Haskell code."
        , "You can install 'cabal-fmt' by running:"
        , ""
        , "  cabal v2-install cabal-fmt --ignore-project --overwrite-policy=always"
        , ""
        , "See: https://github.com/haskell/cabal-fmt#readme"
        ]
      exitFailure
