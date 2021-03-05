module Main where

import Control.Monad (when)
import System.Environment
import Vehicle.Frontend.Parse

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    error "usage: vehicle [INPUT_FILE]"
  let [file] = args
  prog <- parseFile file
  print prog
