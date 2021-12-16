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
  [ testCase "liftFun" $ letInsertionTest
      "(Nat -> Nat) -> (Nat-> Nat)"
      "let y = (Nat -> Nat) in y -> y"

  , testCase "liftNeg" $ letInsertionTest
      "\\(x : Int) -> (- x) + (- x)"
      "\\(x : Int) -> (let y = (- x) in (y + y))"

  , testCase "liftLam" $ letInsertionTest
      "(\\(x : Nat) -> x) ((\\(y : Nat) -> y) 1)"
      "let id = (\\(z : Nat) -> z) in id (id 1)"
  -- , testCase "lambdaEq"   $ equalUpToAlpha True  "\\(x : Nat) -> x" "\\(y : Nat) -> y"
  -- , testCase "lambda2Neq" $ equalUpToAlpha False "\\(x : Nat) -> \\(y : Nat) -> x" "\\(x : Nat) -> \\(y : Nat) -> y"
  -- , testCase "appEq"      $ equalUpToAlpha True  "(\\(x : Nat) -> x) 1" "(\\(y : Nat) -> y) 1"
  ]

letInsertionTest :: Text -> Text -> Assertion
letInsertionTest input expected = do
  let inputExpr    = textToCheckedExpr input
  let expectedExpr = textToCheckedExpr expected

  let result = discardLogger (insertLets (const True) inputExpr)
  --result <- flushLogs Nothing (insertLets (const True) inputExpr)

  -- Need to re-typecheck the result as let-insertion puts a Hole on
  -- each binder type.
  let typedResult = retypeCheckExpr result

  let errorMessage = layoutAsString $
        "Expected the result of let lifting" <+> squotes (pretty input) <+>
        "to be alpha equivalent to" <+> squotes (prettyVerbose expectedExpr) <+>
        "however the result was" <+> squotes (prettyVerbose typedResult)

  assertBool errorMessage (alphaEq expectedExpr typedResult)
