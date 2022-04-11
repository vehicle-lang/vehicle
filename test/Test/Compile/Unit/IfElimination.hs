module Test.Compile.Unit.IfElimination where

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
import Vehicle.Compile.Normalise.IfElimination
import Vehicle.Compile.CoDeBruijnify

import Test.Compile.Utils

--------------------------------------------------------------------------------
-- Let lifting tests

ifEliminationTests :: TestTree
ifEliminationTests = testGroup "LiftAndElim"
  [ testCase "liftIfAnn" $ liftAndEliminateIfsTest
      "(if False then True else False) : Bool"
      "if False then (True : Bool) else (False : Bool)"

  , testCase "liftListIf2" $ liftAndEliminateIfsTest
      "[if True then 1 else 2, if False then 3 else 4] : List Nat"
      "if True \
        \then (if False then ([1, 3] : List Nat) else ([1, 4] : List Nat)) \
        \else (if False then ([2, 3] : List Nat) else ([2, 4] : List Nat))"

  , testCase "liftIfAdd2" $ liftAndEliminateIfsTest
      "(if True then 1 else 2) + (if False then 3 else (4 : Nat))"
      "if True \
        \then (if False then (1 + 3) else (1 + (4 : Nat))) \
        \else (if False then (2 + 3) else (2 + (4 : Nat)))"

  , testCase "elimIfNot" $ liftAndEliminateIfsTest
      "not (if True then False else (True : Bool))"
      "not ((True and False) or (not True and (True : Bool)))"

  , testCase "elimIfIf" $ liftAndEliminateIfsTest
      "if (if True then False else True) then False else (True : Bool)"
      "if ((True and False) or (not True and True)) then False else (True : Bool)"
  ]

liftAndEliminateIfsTest :: Text -> Text -> Assertion
liftAndEliminateIfsTest input expected = do
  let inputExpr    = textToCheckedExpr input
  let expectedExpr = textToCheckedExpr expected

  let result = discardLogger (liftAndEliminateIfs inputExpr)
  --result <- flushLogs Nothing (liftAndEliminateIfs inputExpr)

  -- Need to re-typecheck the result as lifting may put a `Hole` for
  -- the type of each lifted `if`.
  let typedResult = retypeCheckExpr result

  let errorMessage = layoutAsString $
        "Expected the result of if elimination on" <> line <>
          indent 2 (squotes (pretty input)) <> line <>
        "to be alpha equivalent to" <>  line <>
          indent 2 (squotes (prettyFriendly expectedExpr)) <> line <>
        "however the result was" <> line <>
          indent 2 (squotes (prettyFriendly result))

  assertBool errorMessage (alphaEq expectedExpr typedResult)