module Test.Compile.Unit.QuantifierLifting
  ( quantiferLiftingTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Text
import Data.Hashable

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile (typeCheckExpr)
import Vehicle.Compile.AlphaEquivalence
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.QuantifierLifting
import Vehicle.Compile.CoDeBruijnify

import Test.Compile.Utils

--------------------------------------------------------------------------------
-- Quantifier lifting tests

quantiferLiftingTests :: TestTree
quantiferLiftingTests = testGroup "LiftQuantifiers"
  [ liftQuantifiersTest "liftQuantId"
      "exists x . x >= 0 and 1 >= 0"
      "exists x . x >= 0 and 1 >= 0"

  , liftQuantifiersTest "liftQuantSimple"
      "1 >= 0 and (exists x. x >= 0)"
      "exists x . 1 >= 0 and x >= 0"

  , liftQuantifiersTest "liftQuantParallel"
    "1 >= 0 and (exists x . x >= 0) and (exists y . y >= 0)"
    "exists x . exists y . 1 >= 0 and x >= 0 and y >= 0"

  , liftQuantifiersTest "liftQuantSequential"
    "1 >= 0 and (exists x . (x >= 0 and (exists y . y >= x)))"
    "exists x . exists y . 1 >= 0 and x >= 0 and y >= x"

  , liftQuantifiersTest "liftQuantSequentialAndParallel"
    "(exists z . z + 2 >= 0) and (exists x . (x >= 0 and (exists y . y >= x)))"
    "exists z . exists x . exists y . z + 2 >= 0 and x >= 0 and y >= x"
  ]

--------------------------------------------------------------------------------
-- Test implementation

liftQuantifiersTest :: String -> Text -> Text -> TestTree
liftQuantifiersTest testName input expected = do
  let inputExpr    = textToCheckedExpr input
  let expectedExpr = textToCheckedExpr expected

  let result = discardLogs (liftQuantifiers inputExpr)
  --result = traceLogs (liftAndEliminateIfs inputExpr)

  let errorMessage = layoutAsString $
        "Expected the result of quantifier lifting on" <> line <>
          indent 2 (squotes (pretty input)) <> line <>
        "to be alpha equivalent to" <>  line <>
          indent 2 (squotes (prettyFriendly expectedExpr)) <> line <>
        "however the result was" <> line <>
          indent 2 (squotes (prettyFriendly result))

  testCase testName $
    assertBool errorMessage (alphaEq expectedExpr result)