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

import Vehicle.Prelude

-- | Type of Vehicle Frontend type-level name-binding sites.
data Arg ann
  = Arg ann Symbol


-- | The core Tree structure, parameterised by an annotation type so different
-- types of data can be associated with it's parts.
data Expr ann
  -- Kinds
  = Kind
  | Type
  -- Types
  | TForall     ann (NonEmpty (Expr ann)) (Expr ann)
  | TVar        ann Symbol
  | TFun        ann (Type ann) (Type ann)
  | TBool       ann
  | TProp       ann
  | TReal       ann
  | TInt        ann
  | TList       ann (Type ann)
  | TTensor     ann (Type ann) (Type ann)
  | TAdd        ann (Type ann) (Type ann)
  | TLitDim     ann Integer
  | TCons       ann (Type ann) (Type ann)
  | TLitDimList ann (NonEmpty (Type ann))
  -- Expressions
  | EAnn        ann (Expr ann) (Type ann)
  | ELet        ann (NonEmpty  (Decl ann)) (Expr ann)
  | ELam        ann (NonEmpty  (Arg  ann)) (Expr ann)
  | EApp        ann (Expr ann) (Expr ann)
  | EVar        ann Symbol
  | ETyApp      ann (Expr ann) (Type ann)
  | ETyLam      ann (NonEmpty  (Arg  ann)) (Expr ann)
  | EIf         ann (Expr ann) (Expr ann) (Expr ann)
  | EImpl       ann (Expr ann) (Expr ann)
  | EAnd        ann (Expr ann) (Expr ann)
  | EOr         ann (Expr ann) (Expr ann)
  | ENot        ann (Expr ann)
  | ETrue       ann
  | EFalse      ann
  | EEq         ann (Expr ann) (Expr ann)
  | ENeq        ann (Expr ann) (Expr ann)
  | ELe         ann (Expr ann) (Expr ann)
  | ELt         ann (Expr ann) (Expr ann)
  | EGe         ann (Expr ann) (Expr ann)
  | EGt         ann (Expr ann) (Expr ann)
  | EMul        ann (Expr ann) (Expr ann)
  | EDiv        ann (Expr ann) (Expr ann)
  | EAdd        ann (Expr ann) (Expr ann)
  | ESub        ann (Expr ann) (Expr ann)
  | ENeg        ann (Expr ann)
  | ELitInt     ann Integer
  | ELitReal    ann Double
  | ECons       ann (Expr ann) (Expr ann)
  | EAt         ann (Expr ann) (Expr ann)
  | EAll        ann
  | EAny        ann
  | ELitSeq     ann (NonEmpty (Expr ann))

data Ident ann
  = Ident ann Symbol

-- | Type of Vehicle Frontend declaration.
data Decl ann
  = DeclNetw
    ann
    (Ident ann) -- Name of the declared network.
    (Expr  ann) -- Type of the declared network.

  | DeclData
    ann
    (Ident ann) -- Name of the declared dataset.
    (Expr  ann) -- Type of the declared dataset.

  | DefType
    ann
    (Ident ann) -- Name of the type declaration.
    [Arg   ann] -- Args of the type declaration.
    (Expr  ann) -- Body of the type declaration.

  | DefFun
    ann
    (Ident ann) -- Name of the function declaration.
    (Expr  ann) -- Type of the function declaration.
    [Arg   ann] -- Args of the function declaration.
    (Expr  ann) -- Body of the function declaration.

-- | Type of Vehicle programs
newtype Prog ann
  = Main
    (NonEmpty (Decl ann)) -- Sequence of declarations
