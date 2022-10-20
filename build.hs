---------------------------------------------------------------------------------
-- Build script
--
-- This is the Shake build script for Vehicle. It installs all the necessary
-- subcomponents and programs that Vehicle relies on.
---------------------------------------------------------------------------------

{-# LANGUAGE OverloadedLists #-}

import Control.Monad (when, unless)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Version
import System.Directory
import System.IO
import System.Exit
import System.Environment (lookupEnv)

import Development.Shake hiding (doesFileExist)
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

---------------------------------------------------------------------------------
--- Configuration
---------------------------------------------------------------------------------

vehicleExecutableName :: String
vehicleExecutableName = "vehicle"

toolsFolder :: FilePath
toolsFolder = "tools"

binFolder :: FilePath
binFolder = "bin"

ghcVersion :: Version
ghcVersion = [9,0,1]

alexVersion :: Version
alexVersion = [3,2,6]

happyVersion :: Version
happyVersion = [1,20,0]

bnfcVersion :: Version
bnfcVersion = [2,9,4]

srcDirBNFC :: FilePath
srcDirBNFC = "src" </> "bnfc"

genDirHS :: FilePath
genDirHS   = "gen" </> "hs"

bnfcTargets :: [FilePath]
bnfcTargets = bnfcInternalTargets <> bnfcExternalTargets

bnfcInternalTargets :: [FilePath]
bnfcInternalTargets =
  [ genDirHS </> "Vehicle" </> "Internal" </> file
  | file <- ["Lex.x", "Par.y", "ErrM.hs"]
  ]

bnfcInternalGarbage :: [FilePath]
bnfcInternalGarbage =
  [ genDirHS </> "Vehicle" </> "Internal" </> file
  | file <- ["Test.hs", "Skel.hs", "Doc.txt"]
  ]

bnfcExternalTargets :: [FilePath]
bnfcExternalTargets =
  [ genDirHS </> "Vehicle" </> "External" </> file
  | file <- ["Abs.hs", "Lex.x", "Layout.hs", "Par.y", "ErrM.hs"]
  ]

bnfcExternalGarbage :: [FilePath]
bnfcExternalGarbage =
  [ genDirHS </> "Vehicle" </> "External" </> file
  | file <- ["Test.hs", "Skel.hs", "Doc.txt"]
  ]

---------------------------------------------------------------------------------
-- Dependencies with reasonable error messages
---------------------------------------------------------------------------------

requireHaskell :: Action ()
requireHaskell = do
  missingGHC   <- not <$> hasExecutable "ghc"
  missingCabal <- not <$> hasExecutable "cabal"
  when (missingGHC || missingCabal) $ do
    fail "Vehicle requires GHC and Cabal\n\
         \See: https://www.haskell.org/ghcup/"

-- BNFC -- a generator for parsers and printers
requireBNFC :: Action ()
requireBNFC = cabalInstallIfMissing "bnfc" "BNFC" "https://bnfc.digitalgrammars.com/" bnfcVersion

requireAlex :: Action ()
requireAlex = cabalInstallIfMissing "alex" "alex" "https://hackage.haskell.org/package/alex" alexVersion

requireHappy :: Action ()
requireHappy = cabalInstallIfMissing "happy" "happy" "https://hackage.haskell.org/package/happy" happyVersion

requireAgda :: Action ()
requireAgda = do
  missingAgda <- not <$> hasExecutable "agda"
  when missingAgda $ do
    fail "Agda not installed"

requireMarabou :: Action ()
requireMarabou = do
  missingMarabou <- not <$> hasLocalExecutable "Marabou"
  when missingMarabou $ do
    liftIO $ createDirectoryIfMissing False toolsFolder

    let marabouFolder      = toolsFolder </> "marabou"
    let marabouBuildFolder = marabouFolder </> "build"

    let marabouRepoURL = "https://github.com/NeuralNetworkVerification/Marabou"
    command_ [] "git" [ "clone", marabouRepoURL, marabouFolder]
    -- command_ [] "git" [ "-C", marabouFolder, "checkout", "ffd353b"]

    liftIO $ createDirectoryIfMissing False marabouBuildFolder
    command_ [] "cmake" [ marabouFolder, "-B", marabouBuildFolder ]
    command_ [] "cmake" [ "--build", marabouBuildFolder]

    liftIO $ createDirectoryIfMissing False binFolder
    liftIO $ renameFile (marabouBuildFolder </> "Marabou") (binFolder </> "Marabou")

---------------------------------------------------------------------------------
-- Test Vehicle
---------------------------------------------------------------------------------

main :: IO ()
main = shakeArgs shakeOptions $ do

  -------------------------------------------------------------------------------
  -- Initialise project
  -------------------------------------------------------------------------------

  let setupGitHooks = command_ [] "git"
        [ "config"
        , "--local"
        , "core.hooksPath"
        , "hooks/"
        ]

  phony "init" $ do
    requireHaskell
    requireAlex
    requireHappy
    requireBNFC
    need bnfcTargets

  phony "clean" $ do
    liftIO $ removeDirectoryRecursive genDirHS

  phony "init-marabou" $ do
    requireMarabou

  phony "init-agda" $ do
    -- Find the Vehicle executable, erroring if its not installed.
    let err = error "Please install Vehicle before running \"init-agda\""
    vehicleExecutablePath <- liftIO $ fromMaybe err <$> findExecutable vehicleExecutableName

    requireAgda

    putInfo ""
    putInfo "Setting up Agda installation for Vehicle"

    -- Locate the home directory
    homeDirectory <- liftIO getHomeDirectory
    let agdaDirectory = homeDirectory </> ".agda"

    -- Install the Vehicle agda-lib
    agdaLibrariesFile <- liftIO $ makeAbsolute $ agdaDirectory </> "libraries"
    vehicleAgdaLib <- liftIO $ makeAbsolute "./src/agda/vehicle.agda-lib"
    addedLibrary <- addLineToFileIfNotPresent agdaLibrariesFile vehicleAgdaLib

    -- Install the Vehicle executable
    agdaExecutablesFile <- liftIO $ makeAbsolute $ agdaDirectory </> "executables"
    addedExecutable <- addLineToFileIfNotPresent agdaExecutablesFile vehicleExecutablePath

    return ()

  -------------------------------------------------------------------------------
  -- Build parsers for External and Internal languages using BNFC
  -------------------------------------------------------------------------------
  --
  -- NOTE:
  --
  -- The call to BNFC creates multiple files, so we're using a multi-target task.
  -- To keep things readable, we first compute the targets for the External and
  -- the Internal languages, and then define a task for each. The phony bnfc task
  -- builds all parsers.

  phony "bnfc" $ do
    need bnfcTargets

  bnfcInternalTargets &%> \_ -> do
    requireBNFC
    need [ srcDirBNFC </> "Internal.cf" ]
    liftIO $ createDirectoryIfMissing True genDirHS
    command_ [] "bnfc"
      [ "-d"
      , "--haskell"
      , "--generic"
      , "--text-token"
      , "--name-space=Vehicle"
      , "--outputdir=" <> genDirHS
      , srcDirBNFC </> "Internal.cf"
      ]
    removeFilesAfter "." bnfcInternalGarbage

  bnfcExternalTargets &%> \_ -> do
    requireBNFC
    need [ srcDirBNFC </> "External.cf" ]
    liftIO $ createDirectoryIfMissing True genDirHS
    command_ [] "bnfc"
      [ "-d"
      , "--haskell"
      , "--generic"
      , "--text-token"
      , "--name-space=Vehicle"
      , "--outputdir=" <> genDirHS
      , srcDirBNFC </> "External.cf"
      ]
    removeFilesAfter "." bnfcExternalGarbage

{-
.PHONY: bnfc-internal-info
bnfc-internal-info: $(GEN_DIR_HS)/Vehicle/Internal/Par.info

$(GEN_DIR_HS)/Vehicle/Internal/Par.info: $(GEN_DIR_HS)/Vehicle/Internal/Par.y
	$(HAPPY) gen/hs/Vehicle/Internal/Par.y --info=gen/hs/Vehicle/Internal/Par.info
-}

{-
.PHONY: bnfc-external-info
bnfc-external-info: $(GEN_DIR_HS)/Vehicle/External/Par.info

$(GEN_DIR_HS)/Vehicle/External/Par.info: $(GEN_DIR_HS)/Vehicle/External/Par.y
	$(HAPPY) gen/hs/Vehicle/External/Par.y --info=gen/hs/Vehicle/External/Par.info
-}
  -------------------------------------------------------------------------------
  -- Build type-checker and compiler for Vehicle
  -------------------------------------------------------------------------------

  phony "build" $ do
    requireHaskell
    need bnfcTargets
    command_ [] "cabal" ["v2-build"]

  -------------------------------------------------------------------------------
  -- Test Vehicle
  -------------------------------------------------------------------------------

  phony "basic-tests" $ do
    requireHaskell
    need bnfcTargets
    command_ [] "cabal" $
      [ "v2-test"
      , "vehicle-executable-tests"
      ] <>
      testOptions

  phony "basic-tests-accept" $ do
    requireHaskell
    need bnfcTargets
    command_ [] "cabal" $
      [ "v2-test"
      , "vehicle-executable-tests"
      ] <>
      testOptions   <>
      [ "--test-option=--accept" ]

  phony "integration-tests" $ do
    requireHaskell
    need bnfcTargets
    command_ [] "cabal" $
      [ "v2-test"
      , "vehicle-integration-tests"
      ] <>
      testOptions

  phony "benchmark-tests" $ do
    requireHaskell
    need bnfcTargets
    command_ [] "cabal" $
      [ "v2-test"
      , "vehicle-benchmarks"
      ] <>
      testOptions

  phony "all-tests" $ do
    requireHaskell
    need bnfcTargets
    command_ [] "cabal" $
      [ "v2-test"
      ] <>
      testOptions

---------------------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------------------

testOptions :: [String]
testOptions =
  [ "--test-show-details=always"
  , "--test-option=--color=always"
  ]

hasExecutable :: String -> Action Bool
hasExecutable prog = isJust <$> liftIO (findExecutable prog)

hasLocalExecutable :: String -> Action Bool
hasLocalExecutable prog = liftIO (System.Directory.doesFileExist (binFolder </> prog))

isRunningOnCI :: Action Bool
isRunningOnCI = liftIO $
  (Just "true" ==) <$> lookupEnv "CI"

askConsent :: String -> Action ()
askConsent message = do
  ci <- isRunningOnCI
  unless ci $ do
    putInfo message;
    liftIO $ do
      oldBufferMode <- hGetBuffering stdin
      hSetBuffering stdin NoBuffering
      c <- getChar
      putStrLn $ "Input: " <> [c]
      when (c `notElem` "yY") exitSuccess
      hSetBuffering stdin oldBufferMode

cabalInstallIfMissing :: String -> String -> String -> Version -> Action ()
cabalInstallIfMissing executable packageName link version = do
  missing <- not <$> hasExecutable executable
  when missing $ do
    putInfo $ "Vehicle requires " <> packageName
    putInfo $ "See: " <> link

    -- askConsent $ "Would you like to install " <> packageName <> "? [y/N]"

    command_ [] "cabal"
      [ "v2-install"
      , "--ignore-project"
      , "--overwrite-policy=always"
      , packageName <> "-" <> showVersion version
      ]

    p <- liftIO (findExecutable executable)
    putInfo $ "Exec: " <> show p

addLineToFileIfNotPresent :: FilePath -> String -> Action Bool
addLineToFileIfNotPresent filePath line = do
  fileExists <- liftIO $ doesFileExist filePath

  fileLines <-
    if fileExists then
      liftIO $ lines <$> readFile filePath
    else do
      putInfo $ "Creating file'" <> filePath <> "'"
      return []

  let entryInfo = "entry '" <> line <> "' in '" <> filePath <> "'"

  if line `elem` fileLines
    then do
      putInfo $ "Found existing " <> entryInfo
      return False
    else do
      putInfo $ "Adding " <> entryInfo
      liftIO $ writeFile filePath (unlines (fileLines ++ [line]))
      return True