{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Vehicle qualified

--------------------------------------------------------------------------------
-- Main function

main :: IO ()
main = Vehicle.parseAndRun