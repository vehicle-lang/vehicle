{-# LANGUAGE CPP #-}

module Main where

import System.Environment (getArgs)
import Vehicle.Test.Golden.TestSpec.NewTestSpec (newTestSpec)

#if ghcDebug
import GHC.Debug.Stub (withGhcDebug)
#endif

--------------------------------------------------------------------------------
-- Main entry point for vehicle-new-golden-test

main :: IO ()
main = getArgs >>= newTestSpec
