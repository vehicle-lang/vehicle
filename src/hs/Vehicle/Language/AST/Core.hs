{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Language.AST.Core where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Prelude
import Vehicle.Language.AST.Builtin (Builtin)

--------------------------------------------------------------------------------
-- Universes

type UniverseLevel = Int

--------------------------------------------------------------------------------
-- Meta-variables

newtype Meta = MetaVar Int
  deriving (Eq, Ord, Show, Generic)

instance NFData Meta

instance Pretty Meta where
  pretty (MetaVar m) = "?" <> pretty m

--------------------------------------------------------------------------------
-- Variable names

data Name
  = User Symbol  -- User-generated name
  | Machine      -- Automatically generated name
  deriving (Eq, Ord, Show, Generic)

instance NFData Name

instance Pretty Name where
  pretty (User symbol) = pretty symbol
  pretty Machine       = "Machine"

--------------------------------------------------------------------------------
-- Literals

data Literal
  = LNat  Int
  | LInt  Int
  | LRat  Double
  | LBool Bool
  deriving (Eq, Ord, Show, Generic)

instance NFData Literal

instance Pretty Literal where
  pretty = \case
    LNat  x -> pretty x
    LInt  x -> pretty x
    LRat  x -> pretty x
    LBool x -> pretty x

--------------------------------------------------------------------------------
-- Binders

-- | Binder for lambda and let expressions
--
-- The binder stores the optional type annotation in order to ensure
-- reversibility during delaboration, and that as the type annotation was
-- manually provided by the user it never needs to be updated after unification
-- and type-class resolution.
data Binder var ann
  = Binder
    Provenance
    Visibility      -- Whether binding is explicit or inferred
    Name            -- The name of the bound variable
    (Expr var ann)  -- The (optional) type of the bound variable
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData var, NFData ann) => NFData (Binder var ann)

instance HasProvenance (Binder var ann) where
  prov (Binder p _ _ _) = p

instance HasVisibility (Binder var ann) where
  vis (Binder _ v _ _) = v

--------------------------------------------------------------------------------
-- Function arguments

data Arg var ann
  = Arg
    Visibility       -- Is the argument implicit or explicit?
    (Expr var ann)   -- The argument expression
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData var, NFData ann) => NFData (Arg var ann)

instance HasVisibility (Arg var ann) where
  vis (Arg v _) = v

--------------------------------------------------------------------------------
-- Expressions

-- | Type of Vehicle Core expressions.
--
-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.
--
-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.
data Expr var ann

  -- | The type of types. The type @Type l@ has type @Type (l+1)@.
  = Type UniverseLevel

  -- | User annotation
  | Ann
    ann
    (Expr var ann)    -- The term
    (Expr var ann)    -- The type of the term

  -- | Application of one term to another.
  | App
    ann                       -- Annotation.
    (Expr var ann)            -- Function.
    (NonEmpty (Arg  var ann)) -- Arguments.

  -- | Dependent product (subsumes both functions and universal quantification).
  | Pi
    ann               -- Annotation.
    (Binder var ann)  -- The bound name
    (Expr   var ann)  -- (Dependent) result type.

  -- | Terms consisting of constants that are built into the language.
  | Builtin
    ann              -- Annotation.
    Builtin          -- Builtin name.

  -- | Variables that are bound by other expressions
  | Var
    ann              -- Annotation.
    var              -- Variable name.

  -- | A hole in the program.
  | Hole
    Provenance       -- Source of the meta-variable
    Symbol           -- Hole name.

  -- | Unsolved meta variables.
  | Meta
    ann              -- Annotation.
    Meta             -- Meta variable number.

  -- | Let expressions.
  --
  -- NOTE: that the order of the bound expression and the binder is reversed
  -- to better mimic the flow of the context, which makes writing monadic
  -- operations concisely much easier.
  | Let
    ann               -- Annotation.
    (Expr   var ann)  -- Bound expression body.
    (Binder var ann)  -- Bound expression name.
    (Expr   var ann)  -- Expression body.

  -- | Lambda expressions (i.e. anonymous functions).
  | Lam
    ann               -- Annotation.
    (Binder var ann)  -- Bound expression name.
    (Expr   var ann)  -- Expression body.

  -- | Built-in literal values e.g. numbers/booleans.
  | Literal
    ann               -- Annotation.
    Literal           -- Value.

  -- | A sequence of terms for e.g. list literals.
  | Seq
    ann               -- Annotation.
    [Expr var ann]    -- List of expressions.

  -- | A placeholder for a dictionary of builtin type-classes.
  -- At the moment doesn't carry around any meaningful information
  -- as we don't currently have user-defined type-classes. Later
  -- on they will carry around user definitions.
  | PrimDict (Expr var ann)

  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData var, NFData ann) => NFData (Expr var ann)

--------------------------------------------------------------------------------
-- Declarations

data DeclType
  = Network
  | Dataset

instance Pretty DeclType where
  pretty = \case
    Network  -> "network"
    Dataset  -> "dataset"

newtype Identifier = Identifier Symbol
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier s) = pretty s

instance NFData Identifier

-- | Type of top-level declarations.
data Decl var ann
  = DeclNetw
    Provenance      -- Location in source file.
    Identifier      -- Network name.
    (Expr var ann)  -- Network type.
  | DeclData
    Provenance      -- Location in source file.
    Identifier      -- Dataset name.
    (Expr var ann)  -- Dataset type.
  | DefFun
    Provenance      -- Location in source file.
    Identifier      -- Bound function name.
    (Expr var ann)  -- Bound function type.
    (Expr var ann)  -- Bound function body.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData var, NFData ann) => NFData (Decl var ann)

--------------------------------------------------------------------------------
-- Programs

-- | Type of Vehicle Core programs.
newtype Prog var ann
  = Main [Decl var ann] -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData var, NFData ann) => NFData (Prog var ann)

--------------------------------------------------------------------------------
-- Utilities

-- Preserves invariant that we never have two nested Apps
normApp :: Semigroup ann => ann -> Expr var ann -> NonEmpty (Arg var ann) -> Expr var ann
normApp p (App p' fun args') args = App (p' <> p) fun (args' <> args)
normApp p fun                args = App p fun args

normAppList :: Semigroup ann => ann -> Expr var ann -> [Arg var ann] -> Expr var ann
normAppList _   fun []           = fun
normAppList ann fun (arg : args) = normApp ann fun (arg :| args)

-- Derive recursion principles

makeBaseFunctor ''Arg
makeBaseFunctor ''Binder
makeBaseFunctor ''Expr
