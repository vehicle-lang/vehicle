{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Vehicle.Frontend.AST.Core
  ( Tree(..)
  , Kind
  , Type
  , Expr
  , Decl
  , Prog
  , TArg
  , EArg
  ) where

import Data.List.NonEmpty (NonEmpty)
import Prelude (Double, Integer)

import Vehicle.Prelude

-- | The core Tree structure, parameterised by an annotation type so different
-- types of data can be associated with it's parts.
data family Tree (ann :: Sort -> *) (sort :: Sort)


-- | Type of Vehicle Frontend sorts.
type Kind ann = Tree ann 'KIND

infixl 4 `KApp`

data instance Tree (ann :: Sort -> *) 'KIND
    = KApp  (ann 'KIND) (Kind ann) (Kind ann)
    | KFun  (ann 'KIND) (Kind ann) (Kind ann)
    | KType (ann 'KIND)
    | KDim  (ann 'KIND)
    | KList (ann 'KIND)


-- | Type of Vehicle Frontend types.
type Type ann = Tree ann 'TYPE

infixl 4 `TApp`

data instance Tree (ann :: Sort -> *) 'TYPE
    = TForall     (ann 'TYPE) (NonEmpty (TArg ann)) (Type ann)
    | TApp        (ann 'TYPE) (Type ann) (Type ann)
    | TVar        (ann 'TYPE) Symbol
    | TFun        (ann 'TYPE) (Type ann) (Type ann)
    | TBool       (ann 'TYPE)
    | TProp       (ann 'TYPE)
    | TReal       (ann 'TYPE)
    | TInt        (ann 'TYPE)
    | TList       (ann 'TYPE) (Type ann)
    | TTensor     (ann 'TYPE) (Type ann) (Type ann)
    | TAdd        (ann 'TYPE) (Type ann) (Type ann)
    | TLitDim     (ann 'TYPE) Integer
    | TCons       (ann 'TYPE) (Type ann) (Type ann)
    | TLitDimList (ann 'TYPE) (NonEmpty (Type ann))


-- | Type of Vehicle Frontend type-level name-binding sites.
type TArg ann = Tree ann 'TARG

data instance Tree (ann :: Sort -> *) 'TARG
  = TArg (ann 'TARG) Symbol


-- | Type of Vehicle Frontend expressions.
type Expr ann = Tree ann 'EXPR

infixl 4 `EApp`

data instance Tree (ann :: Sort -> *) 'EXPR
    = EAnn     (ann 'EXPR) (Expr ann) (Type ann)
    | ELet     (ann 'EXPR) (NonEmpty (Decl ann)) (Expr ann)
    | ELam     (ann 'EXPR) (NonEmpty (EArg ann)) (Expr ann)
    | EApp     (ann 'EXPR) (Expr ann) (Expr ann)
    | EVar     (ann 'EXPR) Symbol
    | ETyApp   (ann 'EXPR) (Expr ann) (Type ann)
    | ETyLam   (ann 'EXPR) (NonEmpty (TArg ann)) (Expr ann)
    | EIf      (ann 'EXPR) (Expr ann) (Expr ann) (Expr ann)
    | EImpl    (ann 'EXPR) (Expr ann) (Expr ann)
    | EAnd     (ann 'EXPR) (Expr ann) (Expr ann)
    | EOr      (ann 'EXPR) (Expr ann) (Expr ann)
    | ENot     (ann 'EXPR) (Expr ann)
    | ETrue    (ann 'EXPR)
    | EFalse   (ann 'EXPR)
    | EEq      (ann 'EXPR) (Expr ann) (Expr ann)
    | ENeq     (ann 'EXPR) (Expr ann) (Expr ann)
    | ELe      (ann 'EXPR) (Expr ann) (Expr ann)
    | ELt      (ann 'EXPR) (Expr ann) (Expr ann)
    | EGe      (ann 'EXPR) (Expr ann) (Expr ann)
    | EGt      (ann 'EXPR) (Expr ann) (Expr ann)
    | EMul     (ann 'EXPR) (Expr ann) (Expr ann)
    | EDiv     (ann 'EXPR) (Expr ann) (Expr ann)
    | EAdd     (ann 'EXPR) (Expr ann) (Expr ann)
    | ESub     (ann 'EXPR) (Expr ann) (Expr ann)
    | ENeg     (ann 'EXPR) (Expr ann)
    | ELitInt  (ann 'EXPR) Integer
    | ELitReal (ann 'EXPR) Double
    | ECons    (ann 'EXPR) (Expr ann) (Expr ann)
    | EAt      (ann 'EXPR) (Expr ann) (Expr ann)
    | EAll     (ann 'EXPR)
    | EAny     (ann 'EXPR)
    | ELitSeq  (ann 'EXPR) (NonEmpty (Expr ann))


-- | Type of Vehicle Frontend expression-level name-binding sites.
type EArg ann = Tree ann 'EARG

data instance Tree (ann :: Sort -> *) 'EARG
  = EArg (ann 'EARG) Symbol


-- | Type of Vehicle Frontend declaration.
type Decl ann = Tree ann 'DECL

data instance Tree (ann :: Sort -> *) 'DECL
  = DeclNetw   (ann 'DECL) (EArg ann) (Type ann)
  | DeclData   (ann 'DECL) (EArg ann) (Type ann)
  | DefType    (ann 'DECL) (TArg ann) [TArg ann] (Type ann)
  | DefFun     (ann 'DECL) (EArg ann) (Type ann) [EArg ann] (Expr ann)

type Prog ann = Tree ann 'PROG

-- | Type of Vehicle Frontend programs.
data instance Tree (ann :: Sort -> *) 'PROG
  = Main (ann 'PROG) (NonEmpty (Decl ann))
