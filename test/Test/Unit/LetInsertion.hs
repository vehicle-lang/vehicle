module Test.Unit.LetInsertion where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Text
import Data.Hashable

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Language.Print
import Vehicle.Compile (typeCheckExpr)
import Vehicle.Compile.AlphaEquivalence
import Vehicle.Compile.Error.Meaningful
import Vehicle.Compile.Error
import Vehicle.Compile.LetInsertion
import Vehicle.Compile.CoDeBruijnify
import Vehicle.Compile.Type (runTypeCheck)

import Test.Utils

--------------------------------------------------------------------------------
-- Let lifting tests

letInsertionTests :: TestTree
letInsertionTests = testGroup "LetInsertion"
  [ testCase "insertFun" $ letInsertionTest
      "(Nat -> Nat) -> (Nat -> Nat)"
      "let y = (Nat -> Nat) in y -> y"

  , testCase "insertNeg" $ letInsertionTest
      "\\(x : Int) -> (- x) + (- x)"
      "\\(x : Int) -> (let y = (- x) in (y + y))"

  -- Disabled due to bugs in type-checker
  {-
  , testCase "insertLam" $ letInsertionTest
      "(\\(x : Nat) -> x) ((\\(y : Nat) -> y) 1)"
      "let id = (\\(z : Nat) -> z) in id (id 1)"

  , testCase "insertAdd" $ letInsertionTest
      "\\(x : Int) (y : Int) -> (((- x) + (- y)) / ((- x) + (- y))) + (- y)"
      "\\(x : Int) -> (let b = (- x) in (\\(y : Int) -> (let a = (- y) in (let c = (a + b) in (c / c))) + y))"
  -}
  ]

  -- \y -> \x -> ((x+y) / (x+y)) + y

letInsertionTest :: Text -> Text -> Assertion
letInsertionTest input expected = do
  let inputExpr    = textToCheckedExpr input
  let expectedExpr = textToCheckedExpr expected

  let result = discardLogger (insertLets (\_e q -> q > 1) inputExpr)
  --result <- flushLogs Nothing (insertLets (\_e q -> q > 1) inputExpr)

  -- Need to re-typecheck the result as let-insertion puts a Hole on
  -- each binder type.
  let typedResult = retypeCheckExpr result

  let errorMessage = layoutAsString $
        "Expected the result of let lifting" <> line <>
          indent 2 (squotes (pretty input)) <> line <>
        "to be alpha equivalent to" <> line <>
          indent 2 (squotes (prettyFriendly expectedExpr)) <> line <>
        "however the result was" <> line <>
          indent 2 (squotes (prettyFriendly typedResult))

  assertBool errorMessage (alphaEq expectedExpr typedResult)
