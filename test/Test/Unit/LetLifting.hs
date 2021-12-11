module Test.Unit.LetLifting where

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
import Vehicle.Compile.Lift.LetExpr
import Vehicle.Compile.CoDeBruijnify

import Test.Utils

--------------------------------------------------------------------------------
-- Let lifting tests

letLiftTests :: TestTree
letLiftTests = testGroup "Let lifting"
  [ testCase "liftFun"      $ letLifting "(Nat -> Nat) -> (Nat-> Nat)" "let y = (Nat -> Nat) in y -> y"
  -- , testCase "liftPlus"     $ letLifting "\\(x : Nat) -> (x + x) + (x + x)"
  -- , testCase "lambdaEq"   $ equalUpToAlpha True  "\\(x : Nat) -> x" "\\(y : Nat) -> y"
  -- , testCase "lambda2Neq" $ equalUpToAlpha False "\\(x : Nat) -> \\(y : Nat) -> x" "\\(x : Nat) -> \\(y : Nat) -> y"
  -- , testCase "appEq"      $ equalUpToAlpha True  "(\\(x : Nat) -> x) 1" "(\\(y : Nat) -> y) 1"
  ]

letLifting :: Text -> Text -> Assertion
letLifting input expected = do
  -- printHashCode "Nat"
  -- printHashCode "Nat -> Nat"
  -- printHashCode "Nat -> Nat -> Nat"
  -- printHashCode "(Nat -> Nat) -> (Nat-> Nat)"
  let inputExpr    = textToCheckedExpr input
  let expectedExpr = textToCheckedExpr expected
  result <- flushLogs Nothing (letLift (const True) inputExpr)

  let errorMessage = layoutAsString $
        "Expected the result of let lifting" <+> squotes (pretty input) <+>
        "to be alpha equivalent to" <+> squotes (prettyFriendly expectedExpr) <+>
        "however the result was" <+> squotes (prettyFriendly result)

  assertBool errorMessage (alphaEq expectedExpr result)

printHashCode :: Text -> IO ()
printHashCode t = putStrLn $ layoutAsString $
  pretty (hash $ toHashableCoDBExpr $ textToCheckedExpr t) <+> ":" <+> pretty t