module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Options.Applicative (execParser)
import Vehicle (run)
import Vehicle.CommandLine (commandLineOptionsParserInfo)

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = do
  setLocaleEncoding utf8
  options <- execParser commandLineOptionsParserInfo
  run options
