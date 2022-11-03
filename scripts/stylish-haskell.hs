#!/usr/bin/env cabal
{- cabal:
build-depends:
    base       >=4   && <5
  , containers >=0.5 && <0.6
  , directory  >=1   && <2
  , filepath   >=1   && <2
  , process    >=1.2 && <2

default-language:   Haskell2010
ghc-options:        -Wall
-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
import Control.Monad (filterM, forM, unless)
import Data.List (isPrefixOf, partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist, findExecutable)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitWith)
import System.FilePath (isRelative, makeRelative, splitDirectories,
                        takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), proc, waitForProcess,
                       withCreateProcess)

main :: IO ()
main = do
  -- Find 'stylish-haskell':
  stylishHaskell <- findStylishHaskell

  -- Separate out flags from files:
  (flags, files) <- partition ("-" `isPrefixOf`) <$> getArgs

  -- Check that all the file arguments exist:
  notExist <- filterM (fmap not . doesFileExist) files
  unless (null notExist) $ fail $ unlines $
    "Error: vehicle-stylish-haskell was called with non-existent file paths:"
      : ["- " <> file | file <- notExist]

  -- Check that all file paths are relative:
  let notRelative = filter (not . isRelative) files
  unless (null notRelative) $ fail $ unlines $
    "Error: vehicle-stylish-haskell was called with non-relative file paths:"
      : ["- " <> file | file <- notExist]

  -- Get all the project root directories:
  let filesByProjectRoot :: Map FilePath [FilePath]
      filesByProjectRoot = foldr insertByProjectRoot Map.empty files
        where
          insertByProjectRoot file =
            Map.alter (Just . (relativeFile :) . fromMaybe []) projectRoot
            where
              projectRoot = fromMaybe "." .listToMaybe . splitDirectories . takeDirectory $ file
              relativeFile = makeRelative projectRoot file

  -- Check that all project roots have a .stylish-haskell.yaml:
  let projectRoots = Map.keys filesByProjectRoot
  projectRootsWithoutStylishHaskellYaml <-
    filterM (fmap not . doesFileExist . (</> ".stylish-haskell.yaml")) projectRoots
  unless (null projectRootsWithoutStylishHaskellYaml) $ fail $ unlines $
    "Error: vehicle-stylish-haskell was on projects without .stylish-haskell.yaml:"
      : ["- " <> projectRoot | projectRoot <- projectRootsWithoutStylishHaskellYaml]

  -- Call stylish-haskell for each project root:
  exitCodes <-
    forM projectRoots $ \projectRoot ->
      case Map.lookup projectRoot filesByProjectRoot of
        Nothing -> error "Empty key in filesByProjectRoot"
        Just filesForProjectRoot -> do
          let argsForProjectRoot = flags <> filesForProjectRoot
          let callStylishHaskell =
                (proc stylishHaskell argsForProjectRoot)
                  {cwd = Just projectRoot}
          withCreateProcess callStylishHaskell $
            \_stdin _stdout _stderr processHandle -> do
              -- TODO: assert stream handles are Nothing
              waitForProcess processHandle

  -- Exit with success only if all calls to stylish-haskell succeeded
  exitWith $ if all isExitSuccess exitCodes then ExitSuccess else ExitFailure 1

isExitSuccess :: ExitCode -> Bool
isExitSuccess ExitSuccess = True
isExitSuccess _           = False

findStylishHaskell :: IO FilePath
findStylishHaskell = do
  findExecutable "stylish-haskell" >>= \case
    Just stylishHaskell -> return stylishHaskell
    Nothing             -> do
      hPutStrLn stderr
        "Error: The pre-commit hook requires 'stylish-haskell' to format Haskell code.\
        \       You can install 'stylish-haskell' by running:\
        \\
        \       cabal v2-install stylish-haskell --ignore-project --overwrite-policy=always\
        \\
        \       See: https://github.com/haskell/stylish-haskell#readme"
      exitFailure
