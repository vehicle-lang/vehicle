module Test.Compile.Unit.AlphaEquivalence
  ( alphaEquivalenceTests ) where

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
import Vehicle.Compile.CoDeBruijnify

import Test.Utils

--------------------------------------------------------------------------------
-- Alpha equivalence tests

alphaEquivalenceTests :: TestTree
alphaEquivalenceTests = testGroup "AlphaEquivalence"
  [ testCase "natEq"      $ equalUpToAlpha True  "1 : Nat" "1 : Nat"
  , testCase "natNeq"     $ equalUpToAlpha False "1 : Nat" "2 : Nat"
  , testCase "lambdaEq"   $ equalUpToAlpha True  "\\(x : Nat) -> x" "\\(y : Nat) -> y"
  , testCase "lambda2Neq" $ equalUpToAlpha False "\\(x : Nat) -> \\(y : Nat) -> x" "\\(x : Nat) -> \\(y : Nat) -> y"
  , testCase "appEq"      $ equalUpToAlpha True  "(\\(x : Nat) -> x) 1" "(\\(y : Nat) -> y) 1"
  ]

type CBExpr = CoDBExpr CheckedAnn

equalUpToAlpha :: Bool -> Text -> Text -> Assertion
equalUpToAlpha shouldBeEqual t1 t2  = do
  let e1 = textToCheckedExpr t1
  let e2 = textToCheckedExpr t2
  let errorMessage = layoutAsString $
        "Expected:" <+> line <> indent 2 (
          squotes (pretty t1)  <> line <>
          squotes (pretty t2)) <> line <>
        "to" <+> (if shouldBeEqual then "" else "not ") <>
        "be equal up to alpha equivalence" <>
        "\n\n" <>
        "CoDB parses:" <+> line <> indent 2 (
          squotes (prettyVerbose (toCoDBExpr e1)) <> line <>
          squotes (prettyVerbose (toCoDBExpr e2))) <>
        "\n\n" <>
        "Hashes:" <> line <> indent 2 (
          squotes (pretty $ hashDBExpr e1) <> line <>
          squotes (pretty $ hashDBExpr e2))

  let result = alphaEq e1 e2
  assertBool errorMessage (result == shouldBeEqual)