{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Vehicle.Core.AST.Core where

import Numeric.Natural (Natural)
import Data.Functor.Foldable.TH (makeBaseFunctor)

import Vehicle.Prelude (Symbol, Provenance, Visibility, Literal)
import Vehicle.Core.AST.Builtin (Builtin)

-- | Meta-variables
type Meta = Int

-- | Universe levels
type Level = Natural

data Arg binder var ann
  = Arg
    Provenance
    Visibility
    (Expr binder var ann)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Binder for Pi types
data Binder binder var ann
  = Binder
    Provenance
    Visibility             -- Whether binding is explicit or inferred
    binder                 -- The name of the bound variable
    (Expr binder var ann)  -- The type of the bound variable
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- * Abstract syntax tree for Vehicle Core


-- | Type of Vehicle Core expressions.
--
-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.
--
-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.
data Expr binder var ann

  -- | The type of types. The type @Type l@ has type @Type (l+1)@.
  = Type Level

  -- | The type of type-class constraints. It has type @Type 1@.
  | Constraint

  -- | User annotation
  | Ann
    ann
    (Expr binder var ann)    -- The term
    (Expr binder var ann)    -- The type of the term

  -- | Application of one term to another.
  | App
    ann                      -- Annotation.
    (Expr binder var ann)    -- Function.
    (Arg  binder var ann)    -- Argument.

  -- | Dependent product (subsumes both functions and universal quantification).
  | Pi
    ann                      -- Annotation.
    (Binder binder var ann)  -- The bound name
    (Expr   binder var ann)  -- (Dependent) result type.

  -- | Terms consisting of constants that are built into the language.
  | Builtin
    ann                      -- Annotation.
    Builtin                  -- Builtin name.

  -- | Variables that are bound by other expressions
  | Bound
    ann                      -- Annotation.
    var                      -- Variable name.

  -- | Variables that are free within the expression and instead bound by top level declarations, see `Decl`.
  | Free
    ann                      -- Annotation.
    Symbol                   -- Identifer.

  -- | A hole in the program. Differs from meta-variables as they are not typed.
  | Hole Provenance Symbol

  -- | Unsolved meta variables.
  | Meta Meta                -- Meta variable.

  -- | Let expressions.
  | Let
    ann                      -- Annotation.
    (Binder binder var ann)  -- Bound expression name.
    (Expr   binder var ann)  -- Bound expression body.
    (Expr   binder var ann)  -- Expression body.

  -- | Lambda expressions (i.e. anonymous functions).
  | Lam
    ann                      -- Annotation.
    (Binder binder var ann)  -- Bound expression name.
    (Expr   binder var ann)  -- Expression body.

  -- | Built-in literal values e.g. numbers/booleans.
  | Literal
    ann                      -- Annotation.
    Literal                  -- Value.

  -- | A sequence of terms for e.g. list literals.
  | Seq
    ann                      -- Annotation.
    [Expr binder var ann]    -- List of expressions.

  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''Expr

newtype Ident = Ident Symbol
  deriving (Eq, Ord, Show)

-- | Identifiers for top-level declerations
data DeclName = DeclName Provenance Ident
  deriving (Eq, Ord, Show)

-- | Type of top-level declarations.
data Decl binder var ann
  = DeclNetw
    Provenance                 -- Location in source file.
    DeclName                   -- Network name.
    (Expr   binder var ann)    -- Network type.
  | DeclData
    Provenance                 -- Location in source file.
    DeclName                   -- Dataset name.
    (Expr   binder var ann)    -- Dataset type.
  | DefType
    Provenance                 -- Location in source file.
    DeclName                   -- Bound expr synonym name.
    [Binder binder var ann] -- Bound expr synonym arguments.
    (Expr      binder var ann) -- Bound expr synonym body.
  | DefFun
    Provenance                 -- Location in source file.
    DeclName                   -- Bound function name.
    (Expr binder var ann)      -- Bound function type.
    (Expr binder var ann)      -- Bound function body.
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Type of Vehicle Core programs.
newtype Prog binder var ann
  = Main [Decl binder var ann] -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- TODO make this nicer and differentiate why we're calling it (debug vs user error messages)

-- | An annotation that stores both the type of the expression and some other arbitrary annotations.
-- Used post-type checking. Avoids unrestricted type-level recursion.
data RecAnn binder var ann = RecAnn (Expr binder var (RecAnn binder var ann)) ann
  deriving (Show)