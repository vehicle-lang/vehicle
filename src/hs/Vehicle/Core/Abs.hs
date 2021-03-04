-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Vehicle.Core.Abs where

import Prelude (Char, Double, Int, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)

import qualified Data.Text
import qualified Data.Data    as C (Data, Typeable)
import qualified GHC.Generics as C (Generic)

newtype Builtin = Builtin ((Int, Int), Data.Text.Text)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

newtype Name = Name ((Int, Int), Data.Text.Text)
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Kind = KApp Type Type | KCon Builtin
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Type
    = TApp Type Type
    | TForall Name Kind Type
    | TVar Name
    | TCon Builtin
    | TLitNat Integer
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Expr
    = EApp Expr Expr
    | ELam Name Type Expr
    | ETyApp Expr Type
    | ETyLam Name Kind Expr
    | ELet Name Type Expr Expr
    | EAnn Expr Type
    | EVar Name
    | ECon Builtin
    | ELitNat Integer
    | ELitReal Double
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Decl = DeclNetw Name Type | DeclExpr Name Type Expr
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

data Prog = Main [Decl]
  deriving (C.Eq, C.Ord, C.Show, C.Read, C.Data, C.Typeable, C.Generic)

