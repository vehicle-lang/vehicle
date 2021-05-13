{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Core.AST.Core
  ( Sort(..)
  , SSort(..)
  , KnownSort(..)
  , toSort
  , sort
  , Tree(..)
  , Kind
  , Type
  , Expr
  , Decl
  , Prog
  , TArg
  , EArg
  ) where

import Data.Hashable (Hashable(..), hashUsing)

-- * Abstract syntax tree for Vehicle Core

-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.

-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.

-- | Syntactic sorts used in Vehicle Core syntax.
data Sort = KIND | TYPE | EXPR | DECL | PROG | TARG | EARG
  deriving (Eq, Ord, Enum, Show)

instance Hashable Sort where
  hashWithSalt = hashUsing fromEnum

-- | Singleton type for 'Sort'.
data SSort (sort :: Sort) where
  SKIND :: SSort 'KIND
  STYPE :: SSort 'TYPE
  SEXPR :: SSort 'EXPR
  SDECL :: SSort 'DECL
  SPROG :: SSort 'PROG
  STARG :: SSort 'TARG
  SEARG :: SSort 'EARG

deriving instance (Eq (SSort sort))
deriving instance (Ord (SSort sort))
deriving instance (Show (SSort sort))

class KnownSort (sort :: Sort) where
  sortSing :: SSort sort

instance KnownSort 'KIND where sortSing = SKIND
instance KnownSort 'TYPE where sortSing = STYPE
instance KnownSort 'EXPR where sortSing = SEXPR
instance KnownSort 'DECL where sortSing = SDECL
instance KnownSort 'PROG where sortSing = SPROG
instance KnownSort 'TARG where sortSing = STARG
instance KnownSort 'EARG where sortSing = SEARG

toSort :: SSort sort -> Sort
toSort SKIND = KIND
toSort STYPE = TYPE
toSort SEXPR = EXPR
toSort SDECL = DECL
toSort SPROG = PROG
toSort STARG = TARG
toSort SEARG = EARG

sort :: forall sort. KnownSort sort => Sort
sort = toSort (sortSing @sort)

data family Tree (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *) (sort :: Sort)

type Kind name builtin ann = Tree name builtin ann 'KIND

infixl 4 `KApp`

-- | Type of Vehicle Core kinds.
data instance Tree (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *) 'KIND
  = KApp
    (ann 'KIND)             -- ^ Sort Annotation.
    (Kind name builtin ann) -- ^ Function.
    (Kind name builtin ann) -- ^ Argument.
  | KCon
    (ann 'KIND)             -- ^ Sort Annotation.
    (builtin 'KIND)         -- ^ Builtin name.
  | KMeta
    (ann 'KIND)             -- ^ Sort Annotation.
    Integer                 -- ^ Meta variable.

type Type name builtin ann = Tree name builtin ann 'TYPE

infixl 4 `TApp`

-- | Type of Vehicle Core types.
data instance Tree (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *) 'TYPE
  = TForall
    (ann 'TYPE)             -- ^ Sort Annotation.
    (TArg name builtin ann) -- ^ Bound type name.
    (Type name builtin ann) -- ^ Type body.
  | TApp
    (ann 'TYPE)             -- ^ Sort Annotation.
    (Type name builtin ann) -- ^ Function.
    (Type name builtin ann) -- ^ Argument.
  | TVar
    (ann 'TYPE)             -- ^ Sort Annotation.
    (name 'TYPE)            -- ^ Variable name.
  | TCon
    (ann 'TYPE)             -- ^ Sort Annotation.
    (builtin 'TYPE)         -- ^ Builtin name.
  | TLitDim
    (ann 'TYPE)             -- ^ Sort Annotation.
    Integer                 -- ^ Dimension literal.
  | TLitList
    (ann 'TYPE)             -- ^ Sort Annotation.
    [Type name builtin ann] -- ^ List of types.
  | TMeta
    (ann 'TYPE)             -- ^ Sort Annotation.
    Integer                 -- ^ Meta variable

type Expr name builtin ann = Tree name builtin ann 'EXPR

infixl 4 `EApp`

-- | Type of Vehicle Core expressions.
data instance Tree (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *) 'EXPR
  = EAnn
    (ann 'EXPR)             -- ^ Sort Annotation.
    (Expr name builtin ann) -- ^ Expression.
    (Type name builtin ann) -- ^ Expression type.
  | ELet
    (ann 'EXPR)             -- ^ Sort Annotation.
    (EArg name builtin ann) -- ^ Bound expression name.
    (Expr name builtin ann) -- ^ Bound expression body.
    (Expr name builtin ann) -- ^ Expression body.
  | ELam
    (ann 'EXPR)             -- ^ Sort Annotation.
    (EArg name builtin ann) -- ^ Bound expression name.
    (Expr name builtin ann) -- ^ Expression body.
  | EApp
    (ann 'EXPR)             -- ^ Sort Annotation.
    (Expr name builtin ann) -- ^ Function.
    (Expr name builtin ann) -- ^ Argument.
  | EVar
    (ann 'EXPR)             -- ^ Sort Annotation.
    (name 'EXPR)            -- ^ Variable name.
  | ETyApp
    (ann 'EXPR)             -- ^ Sort Annotation.
    (Expr name builtin ann) -- ^ Type function.
    (Type name builtin ann) -- ^ Type argument.
  | ETyLam
    (ann 'EXPR)             -- ^ Sort Annotation.
    (TArg name builtin ann) -- ^ Bound type name.
    (Expr name builtin ann) -- ^ Expression body.
  | ECon
    (ann 'EXPR)             -- ^ Sort Annotation.
    (builtin 'EXPR)         -- ^ Builtin name.
  | ELitInt
    (ann 'EXPR)             -- ^ Sort Annotation.
    Integer                 -- ^ Integer literal.
  | ELitReal
    (ann 'EXPR)             -- ^ Sort Annotation.
    Double                  -- ^ "Real" literal.
  | ELitSeq
    (ann 'EXPR)             -- ^ Sort Annotation.
    [Expr name builtin ann] -- ^ List of expressions.

type Decl name builtin ann = Tree name builtin ann 'DECL

-- | Type of Vehicle Core declaration.
data instance Tree (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *) 'DECL
  = DeclNetw
    (ann 'DECL)             -- ^ Sort Annotation.
    (EArg name builtin ann) -- ^ Network name.
    (Type name builtin ann) -- ^ Network type.
  | DeclData
    (ann 'DECL)             -- ^ Sort Annotation.
    (EArg name builtin ann) -- ^ Dataset name.
    (Type name builtin ann) -- ^ Dataset type.
  | DefType
    (ann 'DECL)             -- ^ Sort Annotation.
    (TArg name builtin ann) -- ^ Bound type synonym name.
    [TArg name builtin ann] -- ^ Bound type synonym arguments.
    (Type name builtin ann) -- ^ Bound type synonym body.
  | DefFun
    (ann 'DECL)             -- ^ Sort Annotation.
    (EArg name builtin ann) -- ^ Bound function name.
    (Type name builtin ann) -- ^ Bound function type.
    (Expr name builtin ann) -- ^ Bound function body.

type Prog name builtin ann = Tree name builtin ann 'PROG

-- | Type of Vehicle Core programs.
data instance Tree (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *)  'PROG
  = Main
    (ann 'PROG)             -- ^ Sort Annotation.
    [Decl name builtin ann] -- ^ List of declarations.

type TArg name builtin ann = Tree name builtin ann 'TARG

-- | Type of Vehicle Core type-level name-binding sites.
data instance Tree (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *) 'TARG
  = TArg
  (ann 'TARG)               -- ^ Sort Annotation.
  (name 'TARG)              -- ^ Type name.

type EArg name builtin ann = Tree name builtin ann 'EARG

-- | Type of Vehicle Core expression-level name-binding sites.
data instance Tree (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *) 'EARG
  = EArg
  (ann 'EARG)               -- ^ Sort Annotation.
  (name 'EARG)              -- ^ Expression name.
