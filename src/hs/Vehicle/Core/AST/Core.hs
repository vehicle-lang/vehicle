{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}

module Vehicle.Core.AST.Core
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
import Vehicle.Core.AST.Builtin

-- * Abstract syntax tree for Vehicle Core

-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.

-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.

data family Tree (name :: Sort -> *) (ann :: Sort -> *) (sort :: Sort)

type Kind name ann = Tree name ann 'KIND

infixl 4 `KApp`

-- | Type of Vehicle Core kinds.
data instance Tree (name :: Sort -> *) (ann :: Sort -> *) 'KIND
  = KApp
    (ann 'KIND)                -- ^ Sort Annotation.
    (Kind name ann)            -- ^ Function.
    (Kind name ann)            -- ^ Argument.
  | KCon
    (ann 'KIND)                -- ^ Sort Annotation.
    (Builtin 'KIND)            -- ^ Builtin name.
  | KMeta
    (ann 'KIND)                -- ^ Sort Annotation.
    Integer                    -- ^ Meta variable.

type Type name ann = Tree name ann 'TYPE

infixl 4 `TApp`

-- | Type of Vehicle Core types.
data instance Tree (name :: Sort -> *) (ann :: Sort -> *) 'TYPE
  = TForall
    (ann 'TYPE)                -- ^ Sort Annotation.
    (TArg name ann)            -- ^ Bound type name.
    (Type name ann)            -- ^ Type body.
  | TApp
    (ann 'TYPE)                -- ^ Sort Annotation.
    (Type name ann)            -- ^ Function.
    (Type name ann)            -- ^ Argument.
  | TVar
    (ann 'TYPE)                -- ^ Sort Annotation.
    (name 'TYPE)               -- ^ Variable name.
  | TCon
    (ann 'TYPE)                -- ^ Sort Annotation.
    (Builtin 'TYPE)            -- ^ Builtin name.
  | TLitDim
    (ann 'TYPE)                -- ^ Sort Annotation.
    Integer                    -- ^ Dimension literal.
  | TLitDimList
    (ann 'TYPE)                -- ^ Sort Annotation.
    (NonEmpty (Type name ann)) -- ^ List of types.
  | TMeta
    (ann 'TYPE)                -- ^ Sort Annotation.
    Integer                    -- ^ Meta variable

type Expr name ann = Tree name ann 'EXPR

infixl 4 `EApp`

-- | Type of Vehicle Core expressions.
data instance Tree (name :: Sort -> *) (ann :: Sort -> *) 'EXPR
  = EAnn
    (ann 'EXPR)                -- ^ Sort Annotation.
    (Expr name ann)            -- ^ Expression.
    (Type name ann)            -- ^ Expression type.
  | ELet
    (ann 'EXPR)                -- ^ Sort Annotation.
    (EArg name ann)            -- ^ Bound expression name.
    (Expr name ann)            -- ^ Bound expression body.
    (Expr name ann)            -- ^ Expression body.
  | ELam
    (ann 'EXPR)                -- ^ Sort Annotation.
    (EArg name ann)            -- ^ Bound expression name.
    (Expr name ann)            -- ^ Expression body.
  | EApp
    (ann 'EXPR)                -- ^ Sort Annotation.
    (Expr name ann)            -- ^ Function.
    (Expr name ann)            -- ^ Argument.
  | EVar
    (ann 'EXPR)                -- ^ Sort Annotation.
    (name 'EXPR)               -- ^ Variable name.
  | ETyApp
    (ann 'EXPR)                -- ^ Sort Annotation.
    (Expr name ann)            -- ^ Type function.
    (Type name ann)            -- ^ Type argument.
  | ETyLam
    (ann 'EXPR)                -- ^ Sort Annotation.
    (TArg name ann)            -- ^ Bound type name.
    (Expr name ann)            -- ^ Expression body.
  | ECon
    (ann 'EXPR)                -- ^ Sort Annotation.
    (Builtin 'EXPR)            -- ^ Builtin name.
  | ELitInt
    (ann 'EXPR)                -- ^ Sort Annotation.
    Integer                    -- ^ Integer literal.
  | ELitReal
    (ann 'EXPR)                -- ^ Sort Annotation.
    Double                     -- ^ "Real" literal.
  | ELitSeq
    (ann 'EXPR)                -- ^ Sort Annotation.
    (NonEmpty (Expr name ann)) -- ^ List of expressions.

type Decl name ann = Tree name ann 'DECL

-- | Type of Vehicle Core declaration.
data instance Tree (name :: Sort -> *) (ann :: Sort -> *) 'DECL
  = DeclNetw
    (ann 'DECL)                -- ^ Sort Annotation.
    (EArg name ann)            -- ^ Network name.
    (Type name ann)            -- ^ Network type.
  | DeclData
    (ann 'DECL)                -- ^ Sort Annotation.
    (EArg name ann)            -- ^ Dataset name.
    (Type name ann)            -- ^ Dataset type.
  | DefType
    (ann 'DECL)                -- ^ Sort Annotation.
    (TArg name ann)            -- ^ Bound type synonym name.
    [TArg name ann]            -- ^ Bound type synonym arguments.
    (Type name ann)            -- ^ Bound type synonym body.
  | DefFun
    (ann 'DECL)                -- ^ Sort Annotation.
    (EArg name ann)            -- ^ Bound function name.
    (Type name ann)            -- ^ Bound function type.
    (Expr name ann)            -- ^ Bound function body.

type Prog name ann = Tree name ann 'PROG

-- | Type of Vehicle Core programs.
data instance Tree (name :: Sort -> *) (ann :: Sort -> *)  'PROG
  = Main
    (ann 'PROG)                -- ^ Sort Annotation.
    (NonEmpty (Decl name ann)) -- ^ List of declarations.

type TArg name ann = Tree name ann 'TARG

-- | Type of Vehicle Core type-level name-binding sites.
data instance Tree (name :: Sort -> *) (ann :: Sort -> *) 'TARG
  = TArg
  (ann 'TARG)                  -- ^ Sort Annotation.
  (name 'TARG)                 -- ^ Type name.

type EArg name ann = Tree name ann 'EARG

-- | Type of Vehicle Core expression-level name-binding sites.
data instance Tree (name :: Sort -> *) (ann :: Sort -> *) 'EARG
  = EArg
  (ann 'EARG)                  -- ^ Sort Annotation.
  (name 'EARG)                 -- ^ Expression name.
