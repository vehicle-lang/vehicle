{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (fail)
import Test.HUnit
import Data.Text
import Vehicle.Abs (Kind(..), Type(..), Expr(..))
import Vehicle.Lex (Token)
import Vehicle.Par (pKind, pType, pExpr, myLexer)
import Vehicle.Print (Print, printTree)
import Vehicle.Layout (resolveLayout)

main :: IO Counts
main = runTestTT . TestList $
  [ TestLabel "parser" parserTests
  ]



-- * Test Parser

parserTests :: Test
parserTests = TestList
  [ TestLabel "pKind" (runParserTests False pKind pKindTests)
  , TestLabel "pType" (runParserTests False pType pTypeTests)
  , TestLabel "pExpr" (runParserTests False pExpr pExprTests)
  ]

pKindTests :: [(Text, Maybe Kind)]
pKindTests =
  [ "*"               |-> KStar
  , "Nat"             |-> KNat
  , "List *"          |-> KApp KList KStar
  , "List (List Nat)" |-> KApp KList (KApp KList KNat)
  ]

pTypeTests :: [(Text, Maybe Type)]
pTypeTests =
  [
  ]

pExprTests :: [(Text, Maybe Expr)]
pExprTests =
  [
  ]


-- ** Helper functions

(|->) :: Text -> b -> (Text, Maybe b)
x |-> y = (x, Just y)

fail :: Text -> (Text, Maybe b)
fail x = (x, Nothing)

type Parser a = [Token] -> Either String a

runParserTests :: (Show a, Eq a) => Bool -> Parser a -> [(Text, Maybe a)] -> Test
runParserTests topLevel p trs = TestList [runParserTest topLevel p t r | (t, r) <- trs]

runParserTest :: (Show a, Eq a) => Bool -> Parser a -> Text -> Maybe a -> Test
runParserTest topLevel p t r = TestCase (assertEqual (unpack t) r (rightToMaybe (runParser topLevel p t)))

runParser :: Bool -> Parser a -> Text -> Either String a
runParser topLevel p t = p (runLexer topLevel t)

runLexer :: Bool -> Text -> [Token]
runLexer topLevel = resolveLayout topLevel . myLexer

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right x) = Just x
