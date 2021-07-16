{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Vehicle.Core.AST.Core
  ( Expr(..)
  , Decl(..)
  , Prog(..)
  , Meta
  , Arg(..)
  , PiBinder(..)
  , LamBinder(..)
  , Literal(..)
  , Ident(..)
  ) where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Sequence (Seq)
import Numeric.Natural (Natural)

import Vehicle.Prelude (Symbol, Provenance, Visibility)
import Vehicle.Core.AST.Builtin (Builtin)

-- | Meta-variables
type Meta = Integer

data Arg binder var ann
  = Arg Visibility (Expr binder var ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Binder for Pi types
data PiBinder binder var ann
  = PiBinder
    ann
    Visibility                   -- Whether binding is explicit or inferred
    (Maybe binder)               -- The (optional) name of the bound variable
    (Expr binder var ann)        -- The type of the bound variable
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Binder for Let and Lambda types
data LamBinder binder var ann
  = LamBinder
    ann
    Visibility                    -- Whether the binding is explicit or inferred
    binder                        -- The name of the bound variable
    (Maybe (Expr binder var ann)) -- The (optional) type of the bound variable
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Literal
  = LitNat  Natural
  | LitInt  Integer
  | LitReal Double
  | LitBool Bool
  deriving (Eq, Ord, Show)

-- * Abstract syntax tree for Vehicle Core


-- | Type of Vehicle Core expressions.
--
-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.
--
-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.
data Expr binder var ann

  -- | The type of types. It has no type of it's own and correspondingly no annotation.
  = Kind

  -- | Application of one term to another.
  | App
    ann                         -- Annotation.
    (Expr binder var ann)       -- Function.
    (Arg  binder var ann)       -- Argument.

  -- | Dependent product (subsumes both functions and universal quantification).
  | Pi
    ann                         -- Annotation.
    (PiBinder binder var ann)   -- The bound name
    (Expr     binder var ann)   -- (Dependent) result type.

  -- | Terms consisting of constants that are built into the language.
  | Builtin
    ann                         -- Annotation.
    Builtin                     -- Builtin name.

  -- | Variables that are bound by other expressions
  | Bound
    ann                         -- Annotation.
    var                         -- Variable name.

  -- | Variables that are free within the expression and instead bound by top level declarations, see `Decl`.
  | Free
    ann                         -- Annotation.
    Symbol                      -- Identifer.

  -- | Unsolved meta variables.
  | Meta
    ann                         -- Annotation.
    Meta                        -- Meta variable.

  -- | Let expressions.
  | Let
    ann                         -- Annotation.
    (LamBinder binder var ann)  -- Bound expression name.
    (Expr      binder var ann)  -- Bound expression body.
    (Expr      binder var ann)  -- Expression body.

  -- | Lambda expressions (i.e. anonymous functions).
  | Lam
    ann                         -- Annotation.
    (LamBinder binder var ann)  -- Bound expression name.
    (Expr      binder var ann)  -- Expression body.

  -- | Built-in literal values e.g. numbers/booleans.
  | Literal
    ann                         -- Annotation.
    Literal                     -- Value.

  -- | A sequence of terms for e.g. list literals.
  | Seq
    ann                         -- Annotation.
    (Seq (Expr binder var ann)) -- List of expressions.

  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Identifiers for top-level declerations
data Ident ann
  = Ident ann Symbol
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Type of top-level declarations.
data Decl binder var ann
  = DeclNetw
    Provenance                 -- Location in source file.
    (Ident ann)                -- Network name.
    (Expr   binder var ann)    -- Network type.
  | DeclData
    Provenance                 -- Location in source file.
    (Ident ann)                -- Dataset name.
    (Expr   binder var ann)    -- Dataset type.
  | DefType
    Provenance                 -- Location in source file.
    (Ident ann)                -- Bound expr synonym name.
    [LamBinder binder var ann] -- Bound expr synonym arguments.
    (Expr      binder var ann) -- Bound expr synonym body.
  | DefFun
    Provenance                 -- Location in source file.
    (Ident ann)                -- Bound function name.
    (Expr binder var ann)      -- Bound function type.
    (Expr binder var ann)      -- Bound function body.
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Type of Vehicle Core programs.
newtype Prog binder var ann
  = Main [Decl binder var ann] -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''Expr