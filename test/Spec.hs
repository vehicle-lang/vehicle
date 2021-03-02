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
  [ TestLabel "pKind" (runParserTests pKind pKindTests)
  , TestLabel "pType" (runParserTests pType pTypeTests)
  , TestLabel "pExpr" (runParserTests pExpr pExprTests)
  ]

pKindTests :: [(Text, Maybe Kind)]
pKindTests =
  [ "*" |-> KStar
  , "Nat" |-> KNat
  , "List *" |-> KApp KList KStar
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

(|->) :: a -> b -> (a, Maybe b)
x |-> y = (x, Just y)

fail :: a -> (a, Maybe b)
fail x = (x, Nothing)

type Parser a = [Token] -> Either String a

runParserTests :: (Show a, Eq a) => Parser a -> [(Text, Maybe a)] -> Test
runParserTests p trs = TestList [runParserTest p t r | (t, r) <- trs]

runParserTest :: (Show a, Eq a) => Parser a -> Text -> Maybe a -> Test
runParserTest p t r = TestCase (assertEqual (unpack t) (rightToMaybe (runParser p t)) r)

runParser :: Parser a -> Text -> Either String a
runParser p t = p (runLexer t)

runLexer :: Text -> [Token]
runLexer = resolveLayout True . myLexer

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right x) = Just x
