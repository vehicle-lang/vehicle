{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Vehicle.Core.Abs where

import Prelude hiding (Real)
import Data.Text (Text)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

type Position = (Int, Int)

data Kind builtin nat real name
        = KApp (Kind builtin nat real name) (Kind builtin nat real name)
        | KCon builtin
        deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Type builtin nat real name
        = TApp (Type builtin nat real name) (Type builtin nat real name)
        | TForall name (Kind builtin nat real name) (Type builtin nat real name)
        | TVar name
        | TCon builtin
        | TLitNat nat
        deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Expr builtin nat real name
        = EApp (Expr builtin nat real name) (Expr builtin nat real name)
        | ELam name (Type builtin nat real name) (Expr builtin nat real name)
        | ETyApp (Expr builtin nat real name) (Type builtin nat real name)
        | ETyLam name (Kind builtin nat real name) (Expr builtin nat real name)
        | ELet name (Type builtin nat real name) (Expr builtin nat real name) (Expr builtin nat real name)
        | EAnn (Expr builtin nat real name) (Type builtin nat real name)
        | EVar name
        | ECon builtin
        | ELitNat nat
        | ELitReal real
        deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

data Decl builtin nat real name
        = DeclNetw name (Type builtin nat real name)
        | DeclExpr name (Type builtin nat real name) (Expr builtin nat real name)
        deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

newtype Prog builtin nat real name
        = Main [Decl builtin nat real name]
        deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)


-- * Compatibility with BNFC parser

newtype TokBuiltin
        = TokBuiltin (Position, Text)
        deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

newtype TokNat
        = TokNat (Position, Text)
        deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

newtype TokReal
        = TokReal (Position, Text)
        deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

newtype TokName
        = TokName (Position, Text)
        deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

type Kind_ = Kind TokBuiltin TokNat TokReal TokName
type Type_ = Type TokBuiltin TokNat TokReal TokName
type Expr_ = Expr TokBuiltin TokNat TokReal TokName
type Decl_ = Decl TokBuiltin TokNat TokReal TokName
type Prog_ = Prog TokBuiltin TokNat TokReal TokName

pattern KApp_ kind1 kind2             = KApp kind1 kind2
pattern KCon_ builtin1                = KCon builtin1
pattern TApp_ type1 type2             = TApp type1 type2
pattern TForall_ name1 kind1 type1    = TForall name1 kind1 type1
pattern TVar_ name1                   = TVar name1
pattern TCon_ builtin1                = TCon builtin1
pattern TLitNat_ nat1                 = TLitNat nat1
pattern EApp_ expr1 expr2             = EApp expr1 expr2
pattern ELam_ name1 type1 expr1       = ELam name1 type1 expr1
pattern ETyApp_ expr1 type1           = ETyApp expr1 type1
pattern ETyLam_ name1 kind1 expr1     = ETyLam name1 kind1 expr1
pattern ELet_ name1 type1 expr1 expr2 = ELet name1 type1 expr1 expr2
pattern EAnn_ expr1 type1             = EAnn expr1 type1
pattern EVar_ name1                   = EVar name1
pattern ECon_ builtin1                = ECon builtin1
pattern ELitNat_ nat1                 = ELitNat nat1
pattern ELitReal_ real1               = ELitReal real1
pattern DeclNetw_ name1 type1         = DeclNetw name1 type1
pattern DeclExpr_ name1 type1 expr1   = DeclExpr name1 type1 expr1
pattern Main_ listDecl1               = Main listDecl1
