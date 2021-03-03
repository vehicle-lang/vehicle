{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (fail)
import Test.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath
import Vehicle.Frontend.Abs (LIdent(..), Kind(..), Type(..), Expr(..), Bound(..), Decl(..))
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
  [ TestLabel "pKind" (runParserTests False pKind pKindTests)
  , TestLabel "pType" (runParserTests False pType pTypeTests)
  , TestLabel "pExpr" (runParserTests False pExpr pExprTests)
  , TestLabel "examples" (runFileTests pFileTests)
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
  [ "Bool"                 |-> TBool
  , "Real"                 |-> TReal
  , "Nat"                  |-> TNat
  , "1312"                 |-> TLitNat 1312
  , fail "-1"
  , "[1, 2, 3, 4]"         |-> TListOf [TLitNat 1, TLitNat 2, TLitNat 3, TLitNat 4]
  , "Cons 1 (Cons 2 Nil)"  |-> TApp (TApp TCons (TLitNat 1)) (TApp (TApp TCons (TLitNat 2)) TNil)
  , "2 + 2"                |-> TAdd (TLitNat 2) (TLitNat 2)
  , "Real -> Bool"         |-> TFun TReal TBool
  , "Real -> Real -> Bool" |-> TFun TReal (TFun TReal TBool)
  , "a"                    |-> TVar (LIdent ((1, 1),"a"))
  , "forall x. [x]"        |-> TForall [(LIdent ((1, 8),"x"))] (TListOf [TVar (LIdent ((1, 12),"x"))])
  ]

pExprTests :: [(Text, Maybe Expr)]
pExprTests =
  [ "let y = x in x" |->
    (ELet [DeclExpr (LIdent ((1,6),"y")) [] (EVar (LIdent ((1,10),"x")))] (EVar (LIdent ((1,16),"x"))))
  , "x ! 0" |->
    (EAt (EVar (LIdent ((1,1),"x"))) (ELitNat 0))
  , "f x ! 0" |->
    (EAt (EApp (EVar (LIdent ((1,1),"f"))) (EVar (LIdent ((1,3),"x")))) (ELitNat 0))
  ]

pFileTests :: [FilePath]
pFileTests =
  [ "examples/andGate.vcl"
  , "examples/mnist.vcl"
  , "examples/shortestPath.vcl"
  ]

-- ** Helper functions

(|->) :: Text -> b -> (Text, Maybe b)
x |-> y = (x, Just y)

fail :: Text -> (Text, Maybe b)
fail x = (x, Nothing)

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
