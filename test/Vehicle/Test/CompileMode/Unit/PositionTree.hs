module Vehicle.Test.CompileMode.Unit.PositionTree
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

import Vehicle.Test.Utils

--------------------------------------------------------------------------------
-- Let lifting tests

positionTreeTests :: MonadTest m => m TestTree
positionTreeTests = testGroup "PositionTree" . mconcat <$> sequence
  [ prefixTests
  , substTests
  ]

prefixTests :: MonadTest m => m [TestTree]
prefixTests = traverse prefixTest
  [ PrefixTestCase
    { _testName = "prefix: x+y || x+y && y"
    , tree1     = Leaf
    , tree2     = Node (There (Here Leaf))
    , remainder = Nothing
    , suffixes  = [Node (There (Here Leaf))]
    }

  , PrefixTestCase
    { _testName = "prefix: (x+y)+y || x+y && y"
    , tree1     = Node (Here Leaf)
    , tree2     = Node (Both (Node (There (Here Leaf))) (Here Leaf))
    , remainder = Just (Node (There (Here Leaf)))
    , suffixes  = [Node (There (Here Leaf))]
    }

  , PrefixTestCase
    { _testName = "prefix: ((x+y)/(x+y))+y || x+y && y"
    , tree1     = Node (Here (Node (Both Leaf                       (Here Leaf))))
    , tree2     = Node (Both (Node (Both (Node (There (Here Leaf))) (Here (Node (There (Here Leaf))))))
                             (Here Leaf))
    , remainder = Just (Node (There (Here Leaf)))
    , suffixes  = replicate 2 (Node (There (Here Leaf)))
    }
  ]

substTests :: MonadTest m => m [TestTree]
substTests = traverse substPosTest
  [ SubstPosTestCase "substPosFun"
      "Int"
      (Node (Both Leaf (Here Leaf)))
      "Nat -> Nat"
      "Int -> Int"

  , SubstPosTestCase "substPosFunLeft"
      "Int"
      (Node (Here Leaf))
      "Nat -> Nat"
      "Int -> Nat"

  , SubstPosTestCase "substPosFunRight"
      "Int"
      (Node (There (Here Leaf)))
      "Nat -> Nat"
      "Nat -> Int"

  , SubstPosTestCase "substPosInt"
      "(1 : Int)"
      (Node (There (Here Leaf)))
      "\\(x : Int) -> - x"
      "\\(x : Int) -> (1 : Int)"

  , SubstPosTestCase "substPosNegInt"
      "(1 : Int)"
      (Node $ There $ Here $ Node $ There $ There $ There $ There $ Here Leaf)
      "\\(x : Int) -> - x"
      "\\(x : Int) -> - (1 : Int)"
  ]

data PrefixTestCase = PrefixTestCase
  { _testName :: String
  , tree1     :: PositionTree
  , tree2     :: PositionTree
  , remainder :: Maybe PositionTree
  , suffixes  :: [PositionTree]
  }

prefixTest :: MonadTest m => PrefixTestCase -> m TestTree
prefixTest (PrefixTestCase testName tree1 tree2 expectedRemainder expectedSuffixes) = do
  let expectedResult = (expectedRemainder, expectedSuffixes)
  let result = stripPrefix tree1 tree2
  let revResult = stripPrefix tree2 tree1

  let errorMessage =
        "Expected prefixes to be equal to " <> show expectedResult <>
        " but was actually " <> show result <> " and the reverse application" <>
        " resulted in " <> show revResult

  return $ testCase testName $ assertBool errorMessage (result == expectedResult)

data SubstPosTestCase = SubstPosTestCase
  { _testName1     :: String
  , valueText      :: Text
  , positions      :: PositionTree
  , exprText       :: Text
  , expectedResult :: Text
  }

substPosTest :: MonadTest m => SubstPosTestCase -> m TestTree
substPosTest (SubstPosTestCase testName valueText positions exprText expectedResult) =
  unitTestCase testName $ do
    value    <- toCoDBExpr <$> typeCheckExpr valueText
    expr     <- toCoDBExpr <$> typeCheckExpr exprText
    expected <- toCoDBExpr <$> typeCheckExpr expectedResult

    let result = substPos value (Just positions) expr

    let errorMessage = layoutAsString $
          "Expected the result of substituting" <+> squotes (pretty valueText) <+>
          "at positions" <> line <>
            indent 2 (squotes (pretty positions)) <> line <>
          "into" <> line <>
            indent 2 (squotes (prettySimple expr)) <> line <>
          "to be" <> line <>
            indent 2 (squotes (prettySimple expected)) <> line <>
          "but found" <> line <>
            indent 2 (squotes (prettySimple result))

    return $ assertBool errorMessage (result == expected)