{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Vehicle.Core.Type where

import Data.Text (Text)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

newtype Builtin = Builtin ((Int, Int), Text)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

newtype Name = Name ((Int, Int), Text)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Kind builtin ann
  = KApp ann (Kind builtin ann) (Kind builtin ann)
  | KCon ann builtin
  | KMeta ann Integer
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Type name builtin ann
  = TForall ann (TArg name builtin ann) (Type name builtin ann)
  | TApp ann (Type name builtin ann) (Type name builtin ann)
  | TVar ann name
  | TCon ann builtin
  | TLitDim ann Integer
  | TLitList ann [Type name builtin ann]
  | TMeta ann Integer
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Expr name builtin ann
  = EAnn ann (Expr name builtin ann) (Type name builtin ann)
  | ELet ann (EArg name builtin ann) (Expr name builtin ann) (Expr name builtin ann)
  | ELam ann (EArg name builtin ann) (Expr name builtin ann)
  | EApp ann (Expr name builtin ann) (Expr name builtin ann)
  | EVar ann name
  | ETyApp ann (Expr name builtin ann) (Type name builtin ann)
  | ETyLam ann (TArg name builtin ann) (Expr name builtin ann)
  | ECon ann builtin
  | ELitInt ann Integer
  | ELitReal ann Double
  | ELitSeq ann [Expr name builtin ann]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Decl name builtin ann
  = DeclNetw ann name (Type name builtin ann)
  | DeclData ann name (Type name builtin ann)
  | DefType ann name [TArg name builtin ann] (Type name builtin ann)
  | DefFun ann name (Type name builtin ann) (Expr name builtin ann)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Prog name builtin ann
  = Main ann [Decl name builtin ann]
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data TArg name builtin ann
  = TArg ann name (Kind builtin ann)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data EArg name builtin ann
  = EArg ann name (Type name builtin ann)
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

type PlainKind = Kind      Builtin ()
type PlainType = Type Name Builtin ()
type PlainExpr = Expr Name Builtin ()
type PlainDecl = Decl Name Builtin ()
type PlainProg = Prog Name Builtin ()
type PlainTArg = TArg Name Builtin ()
type PlainEArg = EArg Name Builtin ()
