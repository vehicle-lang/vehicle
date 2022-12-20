{-# LANGUAGE CPP #-}

module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative (execParser)
import Vehicle (run)
import Vehicle.CommandLine (commandLineOptionsParserInfo)

#if ghcDebug
import GHC.Debug.Stub (withGhcDebug)
#endif

--------------------------------------------------------------------------------
-- Main function

defaultMain :: IO ()
defaultMain = do
  setLocaleEncoding utf8
  options <- execParser commandLineOptionsParserInfo
  run options

--------------------------------------------------------------------------------
-- Main function with ghc-debug instrumentation

#if ghcDebug
main :: IO ()
main = withGhcDebug defaultMain
#else
main :: IO ()
main = defaultMain
#endif
