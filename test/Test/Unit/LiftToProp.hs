module Test.Unit.LiftToProp where

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
import Vehicle.Compile.LiftToProp
import Vehicle.Compile.CoDeBruijnify
import Vehicle.Compile.Type (runTypeCheck)

import Test.Utils

--------------------------------------------------------------------------------
-- Let lifting tests

liftAndElimTests :: TestTree
liftAndElimTests = testGroup "LiftAndElim"
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
      "not ((True => False) and (not True => (True : Bool)))"

  , testCase "elimIfIf" $ liftAndEliminateIfsTest
      "if (if True then False else True) then False else (True : Bool)"
      "if ((True => False) and (not True => True)) then False else (True : Bool)"

  , testCase "liftLetVar" $ liftLetsTest
      "every (x : Prop) . (x  : (let y = Prop in y))"
      "every (x : Prop) . (let y = Prop in (x : y))"

  , testCase "liftLetAnn" $ liftLetsTest
      "(let y = False in y) : Bool"
      "let y = False in (y : Bool)"

  -- , testCase "liftLetSeq2" $ liftLetsTest
  --     "[let (x : Nat) = 1 in x]" -- , let (y : Nat) = 2 in y
  --     "let (x : Nat) = 1 in [x]" --(let (y : Nat) = 2 in [x, y])"
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

liftLetsTest :: Text -> Text -> Assertion
liftLetsTest input expected = do
  let inputExpr    = textToCheckedExpr input
  let expectedExpr = textToCheckedExpr expected

  --print (removeAnnotations expectedExpr)

  let result = discardLogger (liftLets inputExpr)
  --result <- flushLogs Nothing (liftAndEliminateIfs inputExpr)

  --print ("Hi1" <> layoutAsString (prettySimple result))
  -- print ("Hi2" <> layoutAsString (prettySimple expectedExpr))
  -- Need to re-typecheck the result as lifting may put a `Hole` for
  -- the type of each lifted `if`.
  --let typedResult = retypeCheckExpr result

  let errorMessage = layoutAsString $
        "Expected the result of let lifting on" <> line <>
          indent 2 (squotes (pretty input)) <> line <>
        "to be alpha equivalent to" <>  line <>
          indent 2 (squotes (prettySimple expectedExpr)) <> line <>
        "however the result was" <> line <>
          indent 2 (squotes (prettySimple result))

  assertBool errorMessage (alphaEq expectedExpr result)
