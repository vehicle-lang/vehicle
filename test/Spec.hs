{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (fail)
import Test.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath
import Vehicle.Abs (LIdent(..), Kind(..), Type(..), Expr(..), Bound(..), Decl(..))
import Vehicle.Lex (Token)
import Vehicle.Par (pKind, pType, pExpr, pDecl, pProg, myLexer)
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
  , TestLabel "pDecl" (runParserTests False pDecl pDeclTests)
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

pDeclTests :: [(Text, Maybe Decl)]
pDeclTests =
  [ "network f : Tensor Real [2] -> Tensor Real [1]" |->
    DeclNetw (LIdent ((1,9),"f"))
    (TFun
     (TApp (TApp TTensor TReal) (TListOf [TLitNat 2]))
     (TApp (TApp TTensor TReal) (TListOf [TLitNat 1])))
  , "truthy : Real -> Bool" |->
    DeclType (LIdent ((1,1),"truthy"))
    (TFun TReal TBool)
  , "truthy x = x >= 0.5" |->
    DeclExpr (LIdent ((1,1),"truthy")) [LIdent ((1,8),"x")]
    (EGe (EVar (LIdent ((1,12),"x"))) (ELitReal  0.5))
  , "falsey : Real -> Bool" |->
    DeclType (LIdent ((1,1),"falsey"))
    (TFun TReal TBool)
  , "falsey x = x <= 0.5" |->
    DeclExpr (LIdent ((1,1),"falsey")) [LIdent ((1,8),"x")]
    (ELe (EVar (LIdent ((1,12),"x"))) (ELitReal 0.5))
  , "validInput : Tensor Real [2]" |->
    DeclType (LIdent ((1,1),"validInput"))
    (TApp (TApp TTensor TReal) (TListOf [TLitNat 2]))
  , "validInput x = forall xi : x. 0 <= xi && xi <= 1" |->
    DeclExpr (LIdent ((1,1),"validInput")) [LIdent ((1,12),"x")]
    (EForall (LIdent ((1,23),"xi")) (BExpr (EVar (LIdent ((1,28),"x"))))
     (EAnd
      (ELe (ELitNat 0) (EVar (LIdent ((1,36),"xi"))))
      (ELe (EVar (LIdent ((1,42),"xi"))) (ELitNat 1))))
  , "correctOutput : Tensor Real [2] -> Bool" |->
    DeclType (LIdent ((1,1),"correctOutput"))
    (TFun (TApp (TApp TTensor TReal) (TListOf [TLitNat 2])) TBool)
  , T.unlines
    [ "correctOutput x ="
    , "  let y  = f x ! 0 in"
    , "  (truthy x!0 && falsey x!1 => truthy y) &&"
    , "  (truthy x!0 && truthy x!1 => truthy y) &&"
    , "  (falsey x!0 && falsey x!1 => truthy y) &&"
    , "  (falsey x!0 && truthy x!1 => truthy y)"
    ] |->
    DeclExpr (LIdent ((1,1),"correctOutput")) [LIdent ((1,15),"x")]
    (ELet
     [DeclExpr (LIdent ((2,8),"y")) []
      (EAt (EApp (EVar (LIdent ((2,13),"f"))) (EVar (LIdent ((2,15),"x")))) (ELitNat 0))]
     (EAnd
      (EImpl
       (EAnd
        (EAt (EApp (EVar (LIdent ((3,4),"truthy"))) (EVar (LIdent ((3,11),"x")))) (ELitNat 0))
        (EAt (EApp (EVar (LIdent ((3,18),"falsey"))) (EVar (LIdent ((3,25),"x")))) (ELitNat 1)))
       (EApp (EVar (LIdent ((3,32),"truthy"))) (EVar (LIdent ((3,39),"y")))))
      (EAnd
       (EImpl
        (EAnd
         (EAt (EApp (EVar (LIdent ((4,4),"truthy"))) (EVar (LIdent ((4,11),"x")))) (ELitNat 0))
         (EAt (EApp (EVar (LIdent ((4,18),"truthy"))) (EVar (LIdent ((4,25),"x")))) (ELitNat 1)))
        (EApp (EVar (LIdent ((4,32),"truthy"))) (EVar (LIdent ((4,39),"y")))))
       (EAnd
        (EImpl
         (EAnd
          (EAt (EApp (EVar (LIdent ((5,4),"falsey"))) (EVar (LIdent ((5,11),"x")))) (ELitNat 0))
          (EAt (EApp (EVar (LIdent ((5,18),"falsey"))) (EVar (LIdent ((5,25),"x")))) (ELitNat 1)))
         (EApp (EVar (LIdent ((5,32),"truthy"))) (EVar (LIdent ((5,39),"y")))))
        (EImpl
         (EAnd
          (EAt (EApp (EVar (LIdent ((6,4),"falsey"))) (EVar (LIdent ((6,11),"x")))) (ELitNat 0))
          (EAt (EApp (EVar (LIdent ((6,18),"truthy"))) (EVar (LIdent ((6,25),"x")))) (ELitNat 1)))
         (EApp (EVar (LIdent ((6,32),"truthy"))) (EVar (LIdent ((6,39),"y")))))
     ))))
  , "correct : Bool" |->
    DeclType (LIdent ((1,1), "correct"))
    TBool
  , "correct = forall x : validInput. correctOutput x" |->
    DeclExpr (LIdent ((1,1),"correct")) []
    (EForall (LIdent ((1,18),"x")) (BExpr (EVar (LIdent ((1,22),"validInput"))))
     (EApp (EVar (LIdent ((1,34),"correctOutput"))) (EVar (LIdent ((1,48),"x")))))
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
