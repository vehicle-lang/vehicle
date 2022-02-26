{-# LANGUAGE OverloadedStrings #-}

module Test.Compile.Unit where

import Test.Tasty ( testGroup, TestTree )

import Test.Compile.Unit.AlphaEquivalence ( alphaEquivalenceTests )
import Test.Compile.Unit.PositionTree ( positionTreeTests )
import Test.Compile.Unit.LetInsertion ( letInsertionTests )
import Test.Compile.Unit.CoDeBruijn ( coDeBruijnTests )
import Test.Compile.Unit.IfElimination ( ifEliminationTests )


unitTests :: TestTree
unitTests = testGroup "UnitTests"
  [ alphaEquivalenceTests
  , coDeBruijnTests
  , positionTreeTests
  , letInsertionTests
  , ifEliminationTests
  ]