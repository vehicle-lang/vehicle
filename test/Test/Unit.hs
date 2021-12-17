{-# LANGUAGE OverloadedStrings #-}

module Test.Unit where

import Test.Tasty ( testGroup, TestTree )
import Test.Unit.AlphaEquivalence ( alphaEquivalenceTests )
import Test.Unit.PositionTree ( positionTreeTests )
import Test.Unit.LetInsertion ( letInsertionTests )
import Test.Unit.CoDeBruijn ( coDeBruijnTests )
import Test.Unit.LiftToProp ( liftAndElimTests )


unitTests :: TestTree
unitTests = testGroup "UnitTests"
  [ alphaEquivalenceTests
  , coDeBruijnTests
  , positionTreeTests
  , letInsertionTests
  , liftAndElimTests
  ]