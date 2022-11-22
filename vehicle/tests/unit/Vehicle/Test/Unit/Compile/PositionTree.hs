module Vehicle.Test.Unit.Compile.PositionTree
  ( positionTreeTests
  ) where

import Control.Monad (join)
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Vehicle.Compile (parseAndTypeCheckExpr)
import Vehicle.Compile.Print (prettySimple)
import Vehicle.Expr.CoDeBruijn (substPos)
import Vehicle.Expr.CoDeBruijn.Conversion (toCoDBExpr)
import Vehicle.Expr.CoDeBruijn.PositionTree (PositionList (Both, Here, There),
                                             PositionTree (..), stripPrefix)
import Vehicle.Prelude (Pretty (pretty), indent, layoutAsString, line, squotes,
                        (<+>))
import Vehicle.Test.Unit.Common (unitTestCase)

--------------------------------------------------------------------------------
-- Let lifting tests

positionTreeTests :: TestTree
positionTreeTests =
  testGroup "PositionTree" . join $
  [ prefixTests
  , substTests
  ]

prefixTests :: [TestTree]
prefixTests = prefixTest <$>
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

substTests :: [TestTree]
substTests = fmap substPosTest
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

prefixTest :: PrefixTestCase -> TestTree
prefixTest (PrefixTestCase testName tree1 tree2 expectedRemainder expectedSuffixes) =
  testCase testName $ assertBool errorMessage (result == expectedResult)
  where
  expectedResult = (expectedRemainder, expectedSuffixes)
  result = stripPrefix tree1 tree2
  revResult = stripPrefix tree2 tree1
  errorMessage =
        "Expected prefixes to be equal to " <> show expectedResult <>
        " but was actually " <> show result <> " and the reverse application" <>
        " resulted in " <> show revResult

data SubstPosTestCase = SubstPosTestCase
  { _testName1     :: String
  , valueText      :: Text
  , positions      :: PositionTree
  , exprText       :: Text
  , expectedResult :: Text
  }

substPosTest :: SubstPosTestCase -> TestTree
substPosTest (SubstPosTestCase testName valueText positions exprText expectedResult) =
  unitTestCase testName $ do
    value    <- toCoDBExpr <$> parseAndTypeCheckExpr valueText
    expr     <- toCoDBExpr <$> parseAndTypeCheckExpr exprText
    expected <- toCoDBExpr <$> parseAndTypeCheckExpr expectedResult

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
