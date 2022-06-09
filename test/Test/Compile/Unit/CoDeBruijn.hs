module Test.Compile.Unit.CoDeBruijn
  ( coDeBruijnTests ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Text
import Data.Hashable
import Data.IntMap qualified as IntMap

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile (typeCheckExpr)
import Vehicle.Compile.AlphaEquivalence
import Vehicle.Compile.Error
import Vehicle.Compile.CoDeBruijnify

import Test.Compile.Utils

--------------------------------------------------------------------------------
-- Alpha equivalence tests

coDeBruijnTests :: TestTree
coDeBruijnTests = testGroup "CoDeBruijnIndices"
  [ testCase "type"      $ toFromCoDB "Nat"
  , testCase "typeFun"   $ toFromCoDB "Nat -> Nat"
  , testCase "lam"       $ toFromCoDB "\\(x : Nat) -> x"
  , testCase "lam2"      $ toFromCoDB "\\(f : Nat -> Nat) (x : Nat) -> f x"
  , testCase "pi"        $ toFromCoDB "forallT (n : Nat) . Tensor Nat [n]"
  , testCase "neg"       $ toFromCoDB "\\(x : Int) -> - x"
  ]

toFromCoDB :: Text -> Assertion
toFromCoDB e1  = do
  let e2 = textToCheckedExpr e1
  let e3 = toCoDBExpr e2
  let e4 = fromCoDB e3

  let errorMessage = layoutAsString $
        "Expected:" <+> line <> indent 2 (
          squotes (prettyVerbose e2)  <> line <>
          squotes (prettyVerbose e4)) <> line <>
        "to be equal" <>
        "\n\n" <>
        "CoDB parse:" <+> line <> indent 2 (
          squotes (prettyVerbose e3))

  assertBool errorMessage (e2 == e4)