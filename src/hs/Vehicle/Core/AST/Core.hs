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
  , Binder(..)
  , Literal(..)
  ) where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Sequence (Seq)
import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)

import Vehicle.Prelude (Symbol, Provenance)
import Vehicle.Core.AST.Builtin (Builtin)

-- |Meta-variables
type Meta = Integer

type Identifier = Symbol

data Visibility = Explicit | Inferred
  deriving (Eq, Ord, Show)

data Arg binder var ann
  = Arg ann Visibility (Expr binder var ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Binder binder ann
  = Binder ann binder Visibility
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

  -- | Application of one term to another.
  = App
    ann                         -- Annotation.
    (Expr binder var ann)       -- Function.
    (Arg  binder var ann)       -- Argument.

  -- | Dependent product (subsumes both function and forall).
  | Pi
    ann                         -- Annotation.
    (Binder binder ann)         -- The bound name
    (Expr binder var ann)       -- Argument type.
    (Expr binder var ann)       -- (Dependent) result type.

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
    Identifier                  -- Idenitifer.

  -- | Unsolved meta variables.
  | Meta
    ann                         -- Annotation.
    Meta                        -- Meta variable.

  -- | Let expressions.
  | Let
    ann                         -- Annotation.
    (Binder binder     ann)     -- Bound expression name.
    (Expr   binder var ann)     -- Bound expression body.
    (Expr   binder var ann)     -- Expression body.

  -- | Lambda expressions (i.e. anonymous functions).
  | Lam
    ann                         -- Annotation.
    (Binder binder ann)         -- Bound expression name.
    (Expr binder var ann)       -- Expression body.

  -- | Built-in literal values e.g. numbers/booleans.
  | Literal
    ann                         -- Annotation.
    Literal                     -- Value.

  -- | A sequence of terms for e.g. list literals.
  | Seq
    ann                         -- Annotation.
    (Seq (Expr binder var ann)) -- List of expressions.

  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)


-- | Type of Vehicle Core declaration.
data Decl binder var ann
  = DeclNetw
    Provenance                 -- Location in source file.
    (Binder binder     ann)    -- Network name.
    (Expr   binder var ann)    -- Network type.
  | DeclData
    Provenance                 -- Location in source file.
    (Binder binder     ann)    -- Dataset name.
    (Expr   binder var ann)    -- Dataset type.
  | DefType
    Provenance                 -- Location in source file.
    (Binder binder     ann)    -- Bound expr synonym name.
    [Binder binder     ann]    -- Bound expr synonym arguments.
    (Expr   binder var ann)    -- Bound expr synonym body.
  | DefFun
    Provenance                 -- Location in source file.
    (Binder binder   ann)      -- Bound function name.
    (Expr binder var ann)      -- Bound function type.
    (Expr binder var ann)      -- Bound function body.
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Type of Vehicle Core programs.
newtype Prog binder var ann
  = Main (NonEmpty (Decl binder var ann)) -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''Expr