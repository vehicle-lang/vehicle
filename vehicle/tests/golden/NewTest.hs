{-# LANGUAGE CPP #-}

module Main where

import System.Environment (getArgs)
import Vehicle.Test.Golden.TestSpec.NewTestSpec (newTestSpec)

#if ghcDebug
import GHC.Debug.Stub (withGhcDebug)
#endif

--------------------------------------------------------------------------------
-- Main entry point for vehicle-new-golden-test

defaultMain :: IO ()
defaultMain = getArgs >>= newTestSpec


--------------------------------------------------------------------------------
-- Load ghc-debug instrumentation if built with ghc-debug

#if ghcDebug
main :: IO ()
main = withGhcDebug defaultMain
#else
main :: IO ()
main = defaultMain
#endif
