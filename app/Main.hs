{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Vehicle qualified as Vehicle

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = Vehicle.parseAndRun