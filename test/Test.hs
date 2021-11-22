{-# LANGUAGE ImportQualifiedPost #-}

module Test where

import Test.Tasty
import GHC.IO.Encoding

import Test.Golden (goldenTests)
-- import Test.Generative (generativeTests)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ goldenTests
  -- , generativeTests
  ]