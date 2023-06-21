{-# LANGUAGE CPP #-}

module Main where

import Vehicle qualified (main)

#ifdef ghcDebug
import GHC.Debug.Stub (withGhcDebug)
#endif

--------------------------------------------------------------------------------
-- Main function with ghc-debug instrumentation

#ifdef ghcDebug
main :: IO ()
main = withGhcDebug Vehicle.main
#else
main :: IO ()
main = Vehicle.main
#endif
