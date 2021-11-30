{-# LANGUAGE OverloadedStrings #-}

module Test.Unit where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Control.Monad (void)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Text
import Data.Hashable

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.AlphaEquivalence
import Vehicle.Language.Print
import Vehicle.Compile (typeCheckExpr)
import Vehicle.Compile.Error.Meaningful
import Vehicle.Compile.Error


unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ alphaEquivalenceTests
  ]

alphaEquivalenceTests :: TestTree
alphaEquivalenceTests = testGroup "Alpha equivalence"
  [ testCase "natEq"      $ equalUpToAlpha True  "1 : Nat" "1 : Nat"
  , testCase "natNeq"     $ equalUpToAlpha False "1 : Nat" "2 : Nat"
  , testCase "lambdaEq"   $ equalUpToAlpha True  "\\(x : Nat) -> x" "\\(y : Nat) -> y"
  , testCase "lambda2Neq" $ equalUpToAlpha False "\\(x : Nat) -> \\(y : Nat) -> x" "\\(x : Nat) -> \\(y : Nat) -> y"
  , testCase "appEq"      $ equalUpToAlpha True  "(\\(x : Nat) -> x) 1" "(\\(y : Nat) -> y) 1"
  ]

type DBExpr = DeBruijnExpr CheckedAnn
type CBExpr = CodebruijnExpr CheckedAnn

discardState :: ExceptT CompileError Logger a -> a
discardState e = case discardLogger $ runExceptT e of
  Left  x -> error (show (details x))
  Right y -> y

expr :: Text -> DBExpr
expr = discardState . typeCheckExpr

equalUpToAlpha :: Bool -> Text -> Text -> Assertion
equalUpToAlpha shouldBeEqual t1 t2  = do
  let e1 = expr t1
  let e2 = expr t2
  let ce1 = fmap fst (toCodebruijn e1)
  let ce2 = fmap fst (toCodebruijn e2)
  let errorMessage = layoutAsString $
        "Expected:" <+> line <> indent 2 (
          squotes (pretty t1)  <> line <>
          squotes (pretty t2)) <> line <>
        "to" <+> (if shouldBeEqual then "" else "not ") <>
        "be equal up to alpha equivalence" <>
        "\n\n" <>
        "CoDB parses:" <+> line <> indent 2 (
          squotes (pretty $ show ce1) <> line <>
          squotes (pretty $ show ce2)) <>
        "\n\n" <>
        "Hashes:" <> line <> indent 2 (
          squotes (pretty $ hashDB e1) <> line <>
          squotes (pretty $ hashDB e2))



  let result = alphaEq e1 e2
  assertBool errorMessage (result == shouldBeEqual)
