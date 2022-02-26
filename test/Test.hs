{-# LANGUAGE ImportQualifiedPost #-}

module Test where

import Test.Tasty
import GHC.IO.Encoding

import Test.Compile.Golden (goldenTests)
import Test.Compile.Unit (unitTests)
import Test.Compile.Fail (failTests)
-- import Test.Generative (generativeTests)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ goldenTests
  , unitTests
  , failTests
  ]