module Test.Compile.Unit.PositionTree
  ( positionTreeTests
  ) where


import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( assertBool, testCase, Assertion )

import Vehicle.Prelude
import Vehicle.Language.AST
import Vehicle.Compile (typeCheckExpr)
import Vehicle.Compile.CoDeBruijnify ( toCoDBExpr )
import Vehicle.Language.Print

import Test.Compile.Utils

--------------------------------------------------------------------------------
-- Let lifting tests

data PrefixTestCase = PrefixTestCase
  { tree1     :: PositionTree
  , tree2     :: PositionTree
  , remainder :: Maybe PositionTree
  , suffixes  :: [PositionTree]
  }

positionTreeTests :: TestTree
positionTreeTests = testGroup "PositionTree"
  [ testCase "prefix: x+y || x+y && y" $ prefixTest $ PrefixTestCase
    { tree1     = Leaf
    , tree2     = Node (There (Here Leaf))
    , remainder = Nothing
    , suffixes  = [Node (There (Here Leaf))]
    }

  , testCase "prefix: (x+y)+y || x+y && y" $ prefixTest $ PrefixTestCase
    { tree1     = Node (Here Leaf)
    , tree2     = Node (Both (Node (There (Here Leaf))) (Here Leaf))
    , remainder = Just (Node (There (Here Leaf)))
    , suffixes  = [Node (There (Here Leaf))]
    }

  , testCase "prefix: ((x+y)/(x+y))+y || x+y && y" $ prefixTest $ PrefixTestCase
    { tree1     = Node (Here (Node (Both Leaf                       (Here Leaf))))
    , tree2     = Node (Both (Node (Both (Node (There (Here Leaf))) (Here (Node (There (Here Leaf))))))
                             (Here Leaf))
    , remainder = Just (Node (There (Here Leaf)))
    , suffixes  = replicate 2 (Node (There (Here Leaf)))
    }

  , testCase "substPosFun" $ substPosTest
      "Int"
      (Node (Both Leaf (Here Leaf)))
      "Nat -> Nat"
      "Int -> Int"

  , testCase "substPosFunLeft" $ substPosTest
      "Int"
      (Node (Here Leaf))
      "Nat -> Nat"
      "Int -> Nat"

  , testCase "substPosFunRight" $ substPosTest
      "Int"
      (Node (There (Here Leaf)))
      "Nat -> Nat"
      "Nat -> Int"

  , testCase "substPosInt" $ substPosTest
      "(1 : Int)"
      (Node (There (Here Leaf)))
      "\\(x : Int) -> - x"
      "\\(x : Int) -> (1 : Int)"

  , testCase "substPosNegInt" $ substPosTest
      "(1 : Int)"
      (Node (There (Here (Node (There (There (There (Here Leaf))))))))
      "\\(x : Int) -> - x"
      "\\(x : Int) -> - (1 : Int)"
  ]

prefixTest :: PrefixTestCase -> Assertion
prefixTest (PrefixTestCase tree1 tree2 expectedRemainder expectedSuffixes) = do
  let expectedResult = (expectedRemainder, expectedSuffixes)
  let result = stripPrefix tree1 tree2
  let revResult = stripPrefix tree2 tree1

  let errorMessage =
        "Expected prefixes to be equal to " <> show expectedResult <>
        " but was actually " <> show result <> " and the reverse application" <>
        " resulted in " <> show revResult

  assertBool errorMessage (result == expectedResult)

substPosTest :: Text -> PositionTree -> Text -> Text -> Assertion
substPosTest valueText positions exprText expectedResultText = do
  let value = toCoDBExpr $ textToCheckedExpr valueText
  let expr = toCoDBExpr $ textToCheckedExpr exprText
  let expectedResult = toCoDBExpr $ textToCheckedExpr expectedResultText

  let result = substPos value (Just positions) expr

  let errorMessage = layoutAsString $
        "Expected the result of substituting" <+> squotes (pretty valueText) <+>
        "at positions" <> line <>
          indent 2 (squotes (pretty positions)) <> line <>
        "into" <> line <>
          indent 2 (squotes (pretty exprText)) <> line <>
        "to be" <> line <>
          indent 2 (squotes (pretty expectedResultText)) <> line <>
        "but found" <> line <>
          indent 2 (squotes (prettySimple result))

  assertBool errorMessage (result == expectedResult)