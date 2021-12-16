module Test.Unit.PositionTree
  ( positionTreeTests
  ) where

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

import Test.Utils

--------------------------------------------------------------------------------
-- Let lifting tests

data PrefixTestCase = PrefixTestCase
  { tree1  :: PositionTree
  , tree2  :: PositionTree
  , result :: Maybe PositionTree
  }

positionTreeTests :: TestTree
positionTreeTests = testGroup "PositionTree"
  [ testCase "x+y || x+y && y" $ prefixTest $ PrefixTestCase
    { tree1 = Leaf
    , tree2 = Node (There (Here Leaf))
    , result = Just $ Node (There (Here Leaf))
    }

  , testCase "(x+y)+y || x+y && y" $ prefixTest $ PrefixTestCase
    { tree1  = Node (Here Leaf)
    , tree2  = Node (Both (Node (There (Here Leaf))) (Here Leaf))
    , result = Just $ Node (There (Here Leaf))
    }

  , testCase "((x+y)/(x+y))+y || x+y && y" $ prefixTest $ PrefixTestCase
    { tree1 = Node (Here (Node (Both Leaf                       (Here Leaf))))
    , tree2 = Node (Both (Node (Both (Node (There (Here Leaf))) (Here (Node (There (Here Leaf))))))
                         (Here Leaf))
    , result = Just $ Node (There (Here Leaf))
    }
  ]

prefixTest :: PrefixTestCase -> Assertion
prefixTest (PrefixTestCase tree1 tree2 expectedResult) = do
  let result = prefixTree tree1 tree2
  let revResult = prefixTree tree2 tree1

  let errorMessage =
        "Expected prefixes to be equal to " <> show expectedResult <>
        " but was actually " <> show result <> " and the reverse application" <>
        " resulted in " <> show revResult

  assertBool errorMessage (result == expectedResult)
