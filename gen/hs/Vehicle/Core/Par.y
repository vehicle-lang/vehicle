-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.3).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Vehicle.Core.Par
  ( happyError
  , myLexer
  , pBinder
  , pArg
  , pLit
  , pExpr
  , pExpr1
  , pListExpr1
  , pDecl
  , pListDecl
  , pProg
  ) where

import Prelude

import qualified Vehicle.Core.Abs
import Vehicle.Core.Lex
import qualified Data.Text

}

%name pBinder Binder
%name pArg Arg
%name pLit Lit
%name pExpr Expr
%name pExpr1 Expr1
%name pListExpr1 ListExpr1
%name pDecl Decl
%name pListDecl ListDecl
%name pProg Prog
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('               { PT _ (TS _ 1)           }
  ')'               { PT _ (TS _ 2)           }
  ':type'           { PT _ (TS _ 3)           }
  '['               { PT _ (TS _ 4)           }
  ']'               { PT _ (TS _ 5)           }
  'declare-dataset' { PT _ (TS _ 6)           }
  'declare-network' { PT _ (TS _ 7)           }
  'define-fun'      { PT _ (TS _ 8)           }
  'lambda'          { PT _ (TS _ 9)           }
  'let'             { PT _ (TS _ 10)          }
  'pi'              { PT _ (TS _ 11)          }
  '{'               { PT _ (TS _ 12)          }
  '{{'              { PT _ (TS _ 13)          }
  '}'               { PT _ (TS _ 14)          }
  '}}'              { PT _ (TS _ 15)          }
  L_integ           { PT _ (TI $$)            }
  L_BuiltinToken    { PT _ (T_BuiltinToken _) }
  L_NameToken       { PT _ (T_NameToken _)    }
  L_BoolToken       { PT _ (T_BoolToken _)    }
  L_HoleToken       { PT _ (T_HoleToken _)    }
  L_TypeToken       { PT _ (T_TypeToken _)    }
  L_Rational        { PT _ (T_Rational _)     }

%%

Integer :: { Integer }
Integer  : L_integ  { (read (Data.Text.unpack $1)) :: Integer }

BuiltinToken :: { Vehicle.Core.Abs.BuiltinToken }
BuiltinToken  : L_BuiltinToken { Vehicle.Core.Abs.BuiltinToken (mkPosToken $1) }

NameToken :: { Vehicle.Core.Abs.NameToken }
NameToken  : L_NameToken { Vehicle.Core.Abs.NameToken (mkPosToken $1) }

BoolToken :: { Vehicle.Core.Abs.BoolToken }
BoolToken  : L_BoolToken { Vehicle.Core.Abs.BoolToken (mkPosToken $1) }

HoleToken :: { Vehicle.Core.Abs.HoleToken }
HoleToken  : L_HoleToken { Vehicle.Core.Abs.HoleToken (mkPosToken $1) }

TypeToken :: { Vehicle.Core.Abs.TypeToken }
TypeToken  : L_TypeToken { Vehicle.Core.Abs.TypeToken (mkPosToken $1) }

Rational :: { Vehicle.Core.Abs.Rational }
Rational  : L_Rational { Vehicle.Core.Abs.Rational (mkPosToken $1) }

Binder :: { Vehicle.Core.Abs.Binder }
Binder
  : '(' NameToken ':type' Expr ')' { Vehicle.Core.Abs.ExplicitBinder $2 $4 }
  | '{' NameToken ':type' Expr '}' { Vehicle.Core.Abs.ImplicitBinder $2 $4 }
  | '{{' NameToken ':type' Expr '}}' { Vehicle.Core.Abs.InstanceBinder $2 $4 }

Arg :: { Vehicle.Core.Abs.Arg }
Arg
  : Expr1 { Vehicle.Core.Abs.ExplicitArg $1 }
  | '{' Expr '}' { Vehicle.Core.Abs.ImplicitArg $2 }
  | '{{' Expr '}}' { Vehicle.Core.Abs.InstanceArg $2 }

Lit :: { Vehicle.Core.Abs.Lit }
Lit
  : Integer { Vehicle.Core.Abs.LitInt $1 }
  | Rational { Vehicle.Core.Abs.LitRat $1 }
  | BoolToken { Vehicle.Core.Abs.LitBool $1 }

Expr :: { Vehicle.Core.Abs.Expr }
Expr
  : Expr1 ':type' Expr1 { Vehicle.Core.Abs.Ann $1 $3 }
  | 'pi' Binder Expr1 { Vehicle.Core.Abs.Pi $2 $3 }
  | 'let' Binder Expr1 Expr1 { Vehicle.Core.Abs.Let $2 $3 $4 }
  | 'lambda' Binder Expr1 { Vehicle.Core.Abs.Lam $2 $3 }
  | Expr1 Arg { Vehicle.Core.Abs.App $1 $2 }
  | Expr1 { $1 }

Expr1 :: { Vehicle.Core.Abs.Expr }
Expr1
  : TypeToken { Vehicle.Core.Abs.Type $1 }
  | BuiltinToken { Vehicle.Core.Abs.Builtin $1 }
  | NameToken { Vehicle.Core.Abs.Var $1 }
  | Lit { Vehicle.Core.Abs.Literal $1 }
  | '[' ListExpr1 ']' { Vehicle.Core.Abs.LSeq $2 }
  | HoleToken { Vehicle.Core.Abs.Hole $1 }
  | '(' Expr ')' { $2 }

ListExpr1 :: { [Vehicle.Core.Abs.Expr] }
ListExpr1 : {- empty -} { [] } | Expr1 ListExpr1 { (:) $1 $2 }

Decl :: { Vehicle.Core.Abs.Decl }
Decl
  : '(' 'declare-network' NameToken Expr1 ')' { Vehicle.Core.Abs.DeclNetw $3 $4 }
  | '(' 'declare-dataset' NameToken Expr1 ')' { Vehicle.Core.Abs.DeclData $3 $4 }
  | '(' 'define-fun' NameToken Expr1 Expr1 ')' { Vehicle.Core.Abs.DefFun $3 $4 $5 }

ListDecl :: { [Vehicle.Core.Abs.Decl] }
ListDecl : {- empty -} { [] } | Decl ListDecl { (:) $1 $2 }

Prog :: { Vehicle.Core.Abs.Prog }
Prog : '(' ListDecl ')' { Vehicle.Core.Abs.Main $2 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: Data.Text.Text -> [Token]
myLexer = tokens

}

