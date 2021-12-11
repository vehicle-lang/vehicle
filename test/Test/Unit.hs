{-# LANGUAGE OverloadedStrings #-}

module Test.Unit where

import Test.Tasty ( testGroup, TestTree )
import Test.Unit.AlphaEquivalence ( alphaEquivalenceTests )
import Test.Unit.LetLifting ( letLiftTests )
import Test.Unit.CoDeBruijn ( coDeBruijnTests )


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ alphaEquivalenceTests
  , coDeBruijnTests
  --, letLiftTests
  ]