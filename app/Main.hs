module Main where

import GHC.IO.Encoding (utf8, setLocaleEncoding)
import Options.Applicative (execParser)
import System.Environment (getArgs)

import Vehicle (run)
import Vehicle.CommandLine (commandLineOptionsParserInfo)

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = do
  setLocaleEncoding utf8
  options <- execParser commandLineOptionsParserInfo
  run options