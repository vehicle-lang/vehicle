{-# LANGUAGE OverloadedStrings #-}

module Test.Compile.Unit where

import Test.Tasty ( testGroup, TestTree )

import Test.Compile.Utils ( MonadTest )
import Test.Compile.Unit.AlphaEquivalence ( alphaEquivalenceTests )
import Test.Compile.Unit.PositionTree ( positionTreeTests )
import Test.Compile.Unit.LetInsertion ( letInsertionTests )
import Test.Compile.Unit.CoDeBruijn ( coDeBruijnTests )
import Test.Compile.Unit.IfElimination ( ifEliminationTests )
import Test.Compile.Unit.QuantifierLifting ( quantiferLiftingTests )


-- TODO plug through the testing options
unitTests :: MonadTest m => m TestTree
unitTests = return $ testGroup "UnitTests"
  [ alphaEquivalenceTests
  , coDeBruijnTests
  , positionTreeTests
  , letInsertionTests
  , ifEliminationTests
  , quantiferLiftingTests
  ]