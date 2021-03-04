{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (fail)
import Test.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath
import Vehicle.Frontend.Abs
import Vehicle.Frontend.Lex (Token)
import Vehicle.Frontend.Par (pKind, pType, pExpr, pProg, myLexer)
import Vehicle.Frontend.Print (Print, printTree)
import Vehicle.Frontend.Layout (resolveLayout)

main :: IO Counts
main = runTestTT . TestList $
  [ TestLabel "parser" parserTests
  ]

-- * Test parser for Vehicle Frontend

parserTests :: Test
parserTests = TestList
  [ TestLabel "examples" (runFileTests pFileTests)
  ]

pFileTests :: [FilePath]
pFileTests =
  [ "examples/andGate.vcl"
  , "examples/mnist.vcl"
  , "examples/shortestPath.vcl"
  ]

-- ** Helper functions

type Parser a = [Token] -> Either String a

runFileTests :: [FilePath] -> Test
runFileTests files = TestList [ TestLabel file (runFileTest file) | file <- files ]

runFileTest :: FilePath -> Test
runFileTest file = TestCase $ do
  contents <- T.readFile file
  let errOrAst = runParser True pProg contents
  case errOrAst of
    Left  err -> assertFailure err
    Right ast -> return ()

runParserTests :: (Show a, Eq a) => Bool -> Parser a -> [(Text, Maybe a)] -> Test
runParserTests topLevel p trs = TestList [runParserTest topLevel p t r | (t, r) <- trs]

runParserTest :: (Show a, Eq a) => Bool -> Parser a -> Text -> Maybe a -> Test
runParserTest topLevel p t r = TestCase (assertEqual (T.unpack t) r (rightToMaybe (runParser topLevel p t)))

runParser :: Bool -> Parser a -> Text -> Either String a
runParser topLevel p t = p (runLexer topLevel t)

runLexer :: Bool -> Text -> [Token]
runLexer topLevel = resolveLayout topLevel . myLexer

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left  _) = Nothing
rightToMaybe (Right x) = Just x
