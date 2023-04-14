{-# LANGUAGE CPP #-}

module Main where

import Vehicle qualified (main)

#if ghcDebug
import GHC.Debug.Stub (withGhcDebug)
#endif

--------------------------------------------------------------------------------
-- Main function with ghc-debug instrumentation

#if ghcDebug
main :: IO ()
main = withGhcDebug Vehicle.main
#else
main :: IO ()
main = Vehicle.main
#endif
