{-# LANGUAGE ImportQualifiedPost #-}

module Test where

import Test.Tasty

import Test.Golden (goldenTests)
-- import Test.Generative (generativeTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ goldenTests
  -- , generativeTests
  ]