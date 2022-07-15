module Test.Compile.Unit.IfElimination
  ( ifEliminationTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Text
import Data.Hashable

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile (typeCheckExpr, typeCheck)
import Vehicle.Compile.AlphaEquivalence
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.IfElimination
import Vehicle.Compile.CoDeBruijnify

import Test.Compile.Utils

--------------------------------------------------------------------------------
-- If lifting tests

ifEliminationTests :: MonadTest m => m TestTree
ifEliminationTests = testGroup "LiftAndElimIf" <$>
  traverse liftAndEliminateIfsTest
  [ IfTestSpec "liftIfAnn"
      "(if False then True else False) : Bool"
      "if False then (True : Bool) else (False : Bool)"

  , IfTestSpec "liftListIf2"
      "[if True then 1 else 2, if False then 3 else 4] : List Nat"
      "if True \
        \then (if False then ([1, 3] : List Nat) else ([1, 4] : List Nat)) \
        \else (if False then ([2, 3] : List Nat) else ([2, 4] : List Nat))"

  , IfTestSpec "liftIfAdd2"
      "(if True then 1 else 2) + (if False then 3 else 4)"
      "if True \
        \then (if False then (1 + 3) else (1 + 4)) \
        \else (if False then (2 + 3) else (2 + 4))"

  , IfTestSpec "elimIfNot"
      "not (if True then False else (True : Bool))"
      "not ((True and False) or (False and (True : Bool)))"

  , IfTestSpec "elimIfIf"
      "if (if True then False else True) then False else True"
      "if ((True and False) or (False and True)) then False else True"
  ]

--------------------------------------------------------------------------------
-- Test implementations

data IfTestSpec = IfTestSpec String Text Text

liftAndEliminateIfsTest :: MonadTest m => IfTestSpec -> m TestTree
liftAndEliminateIfsTest (IfTestSpec testName input expected) =
  unitTestCase testName $ do
    inputExpr    <- typeCheckExpr input
    expectedExpr <- typeCheckExpr expected
    result       <- eliminateIfs inputExpr

    -- Need to re-typecheck the result as lifting may put a `Hole` for
    -- the type of each lifted `if`.
    typedResult <- typeCheck result

    let errorMessage = layoutAsString $
          "Expected the result of if elimination on" <> line <>
            indent 2 (squotes (pretty input)) <> line <>
          "to be alpha equivalent to" <>  line <>
            indent 2 (squotes (prettyFriendly expectedExpr)) <> line <>
          "however the result was" <> line <>
            indent 2 (squotes (prettyFriendly result))

    return $ assertBool errorMessage $
      alphaEq expectedExpr typedResult