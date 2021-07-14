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

data Arg name binder ann
  = Arg ann Visibility (Expr name binder ann)
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

-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. expr-checking.

-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.

-- | Expr of Vehicle Core expressions.
data Expr name binder ann
  = App
    ann                             -- ^ Annotation.
    (Expr name binder ann)          -- ^ Function.
    (Arg  name binder ann)          -- ^ Argument.
  | Pi
    ann                             -- ^ Annotation.
    (Binder binder ann)
    (Expr name binder ann)          -- ^ Function.
    (Expr name binder ann)          -- ^ Argument.
  | Builtin
    ann                             -- ^ Annotation.
    Builtin                         -- ^ Builtin name.
  | Bound
    ann                             -- ^ Annotation.
    name                            -- ^ Variable name.
  | Free                            -- ^ Top-level definitions
    ann                             -- ^ Annotation.
    Identifier                      -- ^ Idenitifer.
  | Meta
    ann                             -- ^ Annotation.
    Meta                            -- ^ Meta variable.
  | Let
    ann                             -- ^ Annotation.
    (Binder binder ann)             -- ^ Bound expression name.
    (Expr name binder ann)          -- ^ Bound expression body.
    (Expr name binder ann)          -- ^ Expression body.
  | Lam
    ann                             -- ^ Annotation.
    (Binder binder ann)             -- ^ Bound expression name.
    (Expr name binder ann)          -- ^ Expression body.
  | Literal
    ann
    Literal
  | Seq
    ann                             -- ^ Annotation.
    (Seq (Expr name binder ann))    -- ^ List of expressions.
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Expr of Vehicle Core declaration.
data Decl name binder ann
  = DeclNetw
    Provenance                           -- ^ Location in source file.
    (Binder binder ann)                  -- ^ Network name.
    (Expr name binder ann)               -- ^ Network expr.
  | DeclData
    Provenance                           -- ^ Location in source file.
    (Binder binder ann)                  -- ^ Dataset name.
    (Expr name binder ann)               -- ^ Dataset expr.
  | DefType
    Provenance                           -- ^ Location in source file.
    (Binder binder ann)                  -- ^ Bound expr synonym name.
    [Binder binder ann]                  -- ^ Bound expr synonym arguments.
    (Expr name binder ann)               -- ^ Bound expr synonym body.
  | DefFun
    Provenance                           -- ^ Location in source file.
    (Binder binder ann)                  -- ^ Bound function name.
    (Expr name binder ann)               -- ^ Bound function expr.
    (Expr name binder ann)               -- ^ Bound function body.
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Expr of Vehicle Core programs.
newtype Prog name binder ann
  = Main (NonEmpty (Decl name binder ann)) -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''Expr