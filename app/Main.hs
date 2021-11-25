{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Vehicle qualified
import GHC.IO.Encoding

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = do
  setLocaleEncoding utf8
  Vehicle.run