{-# LANGUAGE OverloadedLists #-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Control.Monad (when)
import Data.Maybe (isJust, isNothing)
import Data.Version
import System.Directory
import System.IO
import System.Exit

ghcVersion :: Version
ghcVersion = [9,0,1]

alexVersion :: Version
alexVersion = [3,2,6]

happyVersion :: Version
happyVersion = [1,20,0]

bnfcVersion :: Version
bnfcVersion = [2,9,3]

{-
ORMOLU_VERSION = 0.3.1.0

CABAL  ?= cabal
HAPPY  ?= happy
ORMOLU ?= ormolu
BNFC   ?= bnfc
-}

---------------------------------------------------------------------------------
--- Configuration
---------------------------------------------------------------------------------

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

hasExecutable :: String -> Action Bool
hasExecutable prog = isJust <$> liftIO (findExecutable prog)

askConsent :: String -> Action ()
askConsent message = do
  putInfo message;
  liftIO $ do
    oldBufferMode <- hGetBuffering stdin
    hSetBuffering stdin NoBuffering
    c <- getChar
    putInfo $ "Input: " <> c
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
      [ "v1-install"
      , packageName <> "-" <> showVersion version
      ]

requireAll :: Action ()
requireAll = do
  requireHaskell
  requireAlex
  requireHappy
  requireBNFC
  -- require-ormolu

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

  {-
	-- Ormolu -- a Haskell formatter
	.PHONY: require-ormolu
	require-ormolu:
	ifeq (,$(wildcard $(shell which ormolu)))
		@echo ""
		@echo "Vehicle requires the Ormolu Haskell formatter"
		@echo "See: https://github.com/tweag/ormolu"
		@echo ""
		@echo -n "Would you like to install Ormolu? [y/N] " \
			&& read ans && [ $${ans:-N} = y ] \
			&& $(CABAL) v2-install --ignore-project ormolu-$(ORMOLU_VERSION)
	endif
	-}

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
    requireAll
    --setupGitHooks

  phony "clean" $ do
    liftIO $ removeDirectoryRecursive genDirHS

{-
---------------------------------------------------------------------------------
-- Format code within project
---------------------------------------------------------------------------------

.PHONY: format
format: require-ormolu
	@echo "Format Haskell code using Ormolu"
	@$(ORMOLU) --mode inplace --cabal-default-extensions $(shell git ls-files '*.hs')

.PHONY: format-check
format-check: require-ormolu
	@echo "Check Haskell code using Ormolu"
	@$(ORMOLU) --mode check --cabal-default-extensions $(shell git ls-files '*.hs')
-}

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
      , "--test-options=\"--color=always --accept\""
      ]

