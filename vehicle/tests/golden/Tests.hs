{-# LANGUAGE CPP #-}

module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>))
import Test.Tasty (defaultMain)
import Vehicle.Test.Golden (makeTestTreeFromDirectoryRecursive)

#if ghcDebug
import GHC.Debug.Stub (withGhcDebug)
#endif

--------------------------------------------------------------------------------
-- Main entry point for vehicle-golden-tests

tests :: IO ()
tests = do
  setLocaleEncoding utf8
  testTree <- makeTestTreeFromDirectoryRecursive "Tests" testDirectory
  defaultMain testTree

testDirectory :: FilePath
testDirectory = "tests" </> "golden"

--------------------------------------------------------------------------------
-- Load ghc-debug instrumentation if built with ghc-debug

#if ghcDebug
main :: IO ()
main = withGhcDebug tests
#else
main :: IO ()
main = tests
#endif
