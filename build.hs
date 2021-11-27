{-# LANGUAGE OverloadedLists #-}

import Control.Monad (when, unless)
import Data.Maybe (isJust, isNothing)
import Data.Version
import System.Directory
import System.IO
import System.Exit
import System.Environment (lookupEnv)

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

---------------------------------------------------------------------------------
--- Configuration
---------------------------------------------------------------------------------

ghcVersion :: Version
ghcVersion = [9,0,1]

alexVersion :: Version
alexVersion = [3,2,6]

happyVersion :: Version
happyVersion = [1,20,0]

bnfcVersion :: Version
bnfcVersion = [2,9,3]

srcDirBNFC :: FilePath
srcDirBNFC = "src" </> "bnfc"

genDirHS :: FilePath
genDirHS   = "gen" </> "hs"

bnfcTargets :: [FilePath]
bnfcTargets = bnfcCoreTargets <> bnfcFrontendTargets

bnfcCoreTargets :: [FilePath]
bnfcCoreTargets =
  [ genDirHS </> "Vehicle" </> "Core" </> file
  | file <- ["Lex.x", "Par.y", "ErrM.hs"]
  ]

bnfcCoreGarbage :: [FilePath]
bnfcCoreGarbage =
  [ genDirHS </> "Vehicle" </> "Core" </> file
  | file <- ["Test.hs", "Skel.hs", "Doc.txt"]
  ]

bnfcFrontendTargets :: [FilePath]
bnfcFrontendTargets =
  [ genDirHS </> "Vehicle" </> "Frontend" </> file
  | file <- ["Abs.hs", "Lex.x", "Layout.hs", "Par.y", "ErrM.hs"]
  ]

bnfcFrontendGarbage :: [FilePath]
bnfcFrontendGarbage =
  [ genDirHS </> "Vehicle" </> "Frontend" </> file
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
requireBNFC = installIfMissing "bnfc" "BNFC" "https://bnfc.digitalgrammars.com/" bnfcVersion

requireAlex :: Action ()
requireAlex = installIfMissing "alex" "alex" "https://hackage.haskell.org/package/alex" alexVersion

requireHappy :: Action ()
requireHappy = installIfMissing "happy" "happy" "https://hackage.haskell.org/package/happy" happyVersion

requireAgda :: Action ()
requireAgda = do
  missingAgda <- not <$> hasExecutable "agda"
  when missingAgda $ do
    fail "Agda not installed"


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

  phony "clean" $ do
    liftIO $ removeDirectoryRecursive genDirHS

  phony "init-agda" $ do
    requireAgda

    putInfo ""
    putInfo "Setting up Agda libraries for Vehicle"

    -- Locate the home directory
    homeDirectory <- liftIO getHomeDirectory
    let agdaDirectory = homeDirectory </> ".agda"

    -- Install the Vehicle agda-lib
    agdaLibrariesFile <- liftIO $ makeAbsolute $ agdaDirectory </> "libraries"
    vehicleAgdaLib <- liftIO $ makeAbsolute "./src/agda/vehicle.agda-lib"
    addedLibrary <- addLineToFileIfNotPresent agdaLibrariesFile vehicleAgdaLib

    -- Install the Vehicle executable

    -- let vehicleExecutable = "unknown"
    -- addedExecutable <- addLineToFileIfNotPresent agdaExecutablesFile vehicleExecutable

    return ()

  -------------------------------------------------------------------------------
  -- Build parsers for Frontend and Core languages using BNFC
  -------------------------------------------------------------------------------
  --
  -- NOTE:
  --
  -- The call to BNFC creates multiple files, so we're using a multi-target task.
  -- To keep things readable, we first compute the targets for the Frontend and
  -- the Core languages, and then define a task for each. The phony bnfc task
  -- builds all parsers.

  phony "bnfc" $ do
    need bnfcCoreTargets
    need bnfcFrontendTargets

  bnfcCoreTargets &%> \_ -> do
    requireBNFC
    need [ srcDirBNFC </> "Core.cf" ]
    liftIO $ createDirectoryIfMissing True genDirHS
    command_ [] "bnfc"
      [ "-d"
      , "--haskell"
      , "--generic"
      , "--text-token"
      , "--name-space=Vehicle"
      , "--outputdir=" <> genDirHS
      , srcDirBNFC </> "Core.cf"
      ]
    removeFilesAfter "." bnfcCoreGarbage

  bnfcFrontendTargets &%> \_ -> do
    requireBNFC
    need [ srcDirBNFC </> "Frontend.cf" ]
    liftIO $ createDirectoryIfMissing True genDirHS
    command_ [] "bnfc"
      [ "-d"
      , "--haskell"
      , "--generic"
      , "--text-token"
      , "--name-space=Vehicle"
      , "--outputdir=" <> genDirHS
      , srcDirBNFC </> "Frontend.cf"
      ]
    removeFilesAfter "." bnfcFrontendGarbage

{-
.PHONY: bnfc-core-info
bnfc-core-info: $(GEN_DIR_HS)/Vehicle/Core/Par.info

$(GEN_DIR_HS)/Vehicle/Core/Par.info: $(GEN_DIR_HS)/Vehicle/Core/Par.y
	$(HAPPY) gen/hs/Vehicle/Core/Par.y --info=gen/hs/Vehicle/Core/Par.info
-}

{-
.PHONY: bnfc-frontend-info
bnfc-frontend-info: $(GEN_DIR_HS)/Vehicle/Frontend/Par.info

$(GEN_DIR_HS)/Vehicle/Frontend/Par.info: $(GEN_DIR_HS)/Vehicle/Frontend/Par.y
	$(HAPPY) gen/hs/Vehicle/Frontend/Par.y --info=gen/hs/Vehicle/Frontend/Par.info
-}
  -------------------------------------------------------------------------------
  -- Build type-checker and compiler for Vehicle
  -------------------------------------------------------------------------------

  phony "build" $ do
    requireHaskell
    need bnfcTargets
    command_ [] "cabal" ["build"]

  -------------------------------------------------------------------------------
  -- Test Vehicle
  -------------------------------------------------------------------------------

  phony "test" $ do
    requireHaskell
    need bnfcTargets
    command_ [] "cabal"
      [ "test"
      , "--test-show-details=always"
      , "--test-options=\"--color=always\""
      ]

  phony "test-accept" $ do
    requireHaskell
    need bnfcTargets
    command_ [] "cabal"
      [ "test"
      , "--test-show-details=always"
      , "--test-options=\"--accept\""
      ]

---------------------------------------------------------------------------------
-- Utility functions
---------------------------------------------------------------------------------

hasExecutable :: String -> Action Bool
hasExecutable prog = isJust <$> liftIO (findExecutable prog)

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

installIfMissing :: String -> String -> String -> Version -> Action ()
installIfMissing executable packageName link version = do
  missing <- not <$> hasExecutable executable
  when missing $ do
    putInfo $ "Vehicle requires " <> packageName
    putInfo $ "See: " <> link

    askConsent $ "Would you like to install " <> packageName <> "? [y/N]"

    command_ [] "cabal"
      [ "install"
      , "--ignore-project"
      , "--overwrite-policy=always"
      , packageName <> "-" <> showVersion version
      ]

    p <- liftIO (findExecutable executable)
    putInfo $ "Exec: " <> show p

addLineToFileIfNotPresent :: FilePath -> String -> Action Bool
addLineToFileIfNotPresent filePath line = do
  fileLines <- liftIO $ lines <$> readFile filePath
  let entryInfo = "entry '" <> line <> "' in " <> filePath

  if line `elem` fileLines
    then do
      putInfo $ "Found existing " <> entryInfo
      return False
    else do
      putInfo $ "Adding " <> entryInfo
      liftIO $ writeFile filePath (unlines (fileLines ++ [line]))
      return True