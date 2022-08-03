module Vehicle.Test.CompileMode.Unit.QuantifierLifting
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

import Vehicle.Test.Utils


--------------------------------------------------------------------------------
-- Quantifier lifting tests

quantiferLiftingTests :: MonadTest m => m TestTree
quantiferLiftingTests = testGroup "LiftQuantifiers" <$>
  traverse liftQuantifiersTest
  [ QuantifierTestSpec "liftQuantId"
      "exists (x : Nat) . x >= 0 and 1 >= 0"
      "exists (x : Nat) . x >= 0 and 1 >= 0"

  , QuantifierTestSpec "liftQuantSimple"
      "1 >= 0 and (exists (x : Nat) . x >= 0)"
      "exists (x : Nat) . 1 >= 0 and x >= 0"

  , QuantifierTestSpec "liftQuantParallel"
      "1 >= 0 and (exists (x : Nat) . x >= 0) and (exists (y : Nat) . y >= 0)"
      "exists (x : Nat) . exists (y : Nat) . 1 >= 0 and x >= 0 and y >= 0"

  , QuantifierTestSpec "liftQuantSequential"
      "1 >= 0 and (exists x . (x >= 0 and (exists y . y >= x)))"
      "exists x . exists y . 1 >= 0 and x >= 0 and y >= x"

  , QuantifierTestSpec "liftQuantSequentialAndParallel"
      "(exists z . z + 2 >= 0) and (exists x . (x >= 0 and (exists y . y >= x)))"
      "exists z . exists x . exists y . z + 2 >= 0 and x >= 0 and y >= x"
  ]

--------------------------------------------------------------------------------
-- Test implementation

data QuantifierTestSpec = QuantifierTestSpec String Text Text

liftQuantifiersTest :: MonadTest m => QuantifierTestSpec -> m TestTree
liftQuantifiersTest (QuantifierTestSpec testName input expected) =
    unitTestCase testName $ do
      inputExpr    <- typeCheckExpr input
      expectedExpr <- typeCheckExpr expected
      result       <- liftQuantifiers inputExpr

      let errorMessage = layoutAsString $
            "Expected the result of quantifier lifting on" <> line <>
              indent 2 (squotes (pretty input)) <> line <>
            "to be alpha equivalent to" <>  line <>
              indent 2 (squotes (prettyFriendly expected)) <> line <>
            "however the result was" <> line <>
              indent 2 (squotes (prettyFriendly result))

      return $ assertBool errorMessage $
        alphaEq inputExpr inputExpr