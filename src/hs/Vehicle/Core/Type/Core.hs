{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE PatternSynonyms #-}

module Vehicle.Core.Type.Core
  ( Sort(..)
  , SSort(..)
  , Tree(..)
  , Kind
  , Type
  , Expr
  , Decl
  , Prog
  , TArg
  , EArg
  , NoAnn(..)
  , pattern NA
  ) where

-- * Abstract syntax tree for Vehicle Core

-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.

-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.

-- | Syntactic sorts used in Vehicle Core syntax.
data Sort = KIND | TYPE | EXPR | DECL | PROG | TARG | EARG

-- | Singleton type for 'Sort'.
data SSort (sort :: Sort) where
  SKIND :: SSort 'KIND
  STYPE :: SSort 'TYPE
  SEXPR :: SSort 'EXPR
  SDECL :: SSort 'DECL
  SPROG :: SSort 'PROG
  STARG :: SSort 'TARG
  SEARG :: SSort 'EARG

data family Tree (sort :: Sort) (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *)

type Kind name builtin ann = Tree 'KIND name builtin ann

infixl 4 `KApp`

-- | Type of Vehicle Core kinds.
data instance Tree 'KIND (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *)
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

type Type name builtin ann = Tree 'TYPE name builtin ann

infixl 4 `TApp`

-- | Type of Vehicle Core types.
data instance Tree 'TYPE (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *)
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

type Expr name builtin ann = Tree 'EXPR name builtin ann

infixl 4 `EApp`

-- | Type of Vehicle Core expressions.
data instance Tree 'EXPR (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *)
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

type Decl name builtin ann = Tree 'DECL name builtin ann

-- | Type of Vehicle Core declaration.
data instance Tree 'DECL (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *)
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

type Prog name builtin ann = Tree 'PROG name builtin ann

-- | Type of Vehicle Core programs.
data instance Tree 'PROG (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *)
  = Main
    (ann 'PROG)             -- ^ Sort Annotation.
    [Decl name builtin ann] -- ^ List of declarations.

type TArg name builtin ann = Tree 'TARG name builtin ann

-- | Type of Vehicle Core type-level name-binding sites.
data instance Tree 'TARG (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *)
  = TArg
  (ann 'TARG)               -- ^ Sort Annotation.
  (name 'TARG)              -- ^ Type name.

type EArg name builtin ann = Tree 'EARG name builtin ann

-- | Type of Vehicle Core expression-level name-binding sites.
data instance Tree 'EARG (name :: Sort -> *) (builtin :: Sort -> *) (ann :: Sort -> *)
  = EArg
  (ann 'EARG)               -- ^ Sort Annotation.
  (name 'EARG)              -- ^ Expression name.

-- | Unit annotation.
newtype NoAnn (sort :: Sort) = NoAnn ()
  deriving (Eq, Ord, Show, Read)

pattern NA :: NoAnn sort
pattern NA = NoAnn ()
