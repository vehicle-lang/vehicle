{-# LANGUAGE ImportQualifiedPost #-}

module Test where

import Test.Tasty

import Vehicle
import Test.Golden (goldenTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [goldenTests]