{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Language.AST.Core where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Hashable (Hashable)

import Vehicle.Prelude
import Vehicle.Language.AST.Builtin (Builtin)
import Vehicle.Language.AST.Visibility

--------------------------------------------------------------------------------
-- Universes

type UniverseLevel = Int

--------------------------------------------------------------------------------
-- Meta-variables

newtype Meta = MetaVar Int
  deriving (Eq, Ord, Show, Generic)

instance NFData   Meta
instance Hashable Meta

instance Pretty Meta where
  pretty (MetaVar m) = "?" <> pretty m

--------------------------------------------------------------------------------
-- Literals

-- | Type of literals.
-- - The rational literals should `Ratio`, not `Double`
-- - There should be a family of `Float` literals, but we haven't got there yet.
-- - There is no Real literal as Rationals already cover everything that is
-- expressible
data Literal
  = LBool Bool
  | LNat  Int
  | LInt  Int
  | LRat  Rational
  deriving (Eq, Ord, Show, Generic)

instance NFData   Literal
instance Hashable Literal

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
data Binder binder var ann
  = Binder
    ann
    Visibility            -- The visibility of the binder
    binder                -- The representation of the bound variable
    (Expr binder var ann) -- The (optional) type of the bound variable
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

pattern ExplicitBinder :: ann -> binder -> Expr binder var ann -> Binder binder var ann
pattern ExplicitBinder p n t = Binder p Explicit n t

pattern ImplicitBinder :: ann -> binder -> Expr binder var ann -> Binder binder var ann
pattern ImplicitBinder p n t = Binder p Implicit n t

pattern InstanceBinder :: ann -> binder -> Expr binder var ann -> Binder binder var ann
pattern InstanceBinder p n t = Binder p Instance n t

instance (NFData binder, NFData var, NFData ann) => NFData (Binder binder var ann)

instance HasProvenance ann => HasProvenance (Binder binder var ann) where
  provenanceOf (Binder ann _ _ _) = provenanceOf ann

instance HasOwner ann => HasOwner (Binder binder var ann) where
  ownerOf (Binder ann _ _ _) = ownerOf ann

instance HasVisibility (Binder binder var ann) where
  visibilityOf (Binder _ visibility _ _) = visibility

mapBinderType :: (Expr binder var1 ann -> Expr binder var2 ann)
              -> Binder binder var1 ann -> Binder binder var2 ann
mapBinderType f (Binder ann v n e) = Binder ann v n $ f e

replaceBinderType :: Expr binder var1 ann
                  -> Binder binder var2 ann
                  -> Binder binder var1 ann
replaceBinderType e = mapBinderType (const e)

traverseBinderType :: Monad m
                   => (Expr binder var1 ann -> m (Expr binder var2 ann))
                   -> Binder binder var1 ann
                   -> m (Binder binder var2 ann)
traverseBinderType f (Binder ann v n e) = Binder ann v n <$> f e

--------------------------------------------------------------------------------
-- Function arguments

data Arg binder var ann
  = Arg
    ann                    -- Has the argument been auto-inserted by the type-checker?
    Visibility             -- The visibility of the argument
    (Expr binder var ann)  -- The argument expression
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- At the moment explicit arguments can only ever be provided by the user.
pattern ExplicitArg :: ann -> Expr binder var ann -> Arg binder var ann
pattern ExplicitArg ann e = Arg ann Explicit e

pattern ImplicitArg :: ann -> Expr binder var ann -> Arg binder var ann
pattern ImplicitArg ann e = Arg ann Implicit e

pattern InstanceArg :: ann -> Expr binder var ann -> Arg binder var ann
pattern InstanceArg ann e = Arg ann Instance e

instance (NFData binder, NFData var, NFData ann) => NFData (Arg binder var ann)

instance HasVisibility (Arg binder var ann) where
  visibilityOf (Arg _ v _) = v

instance HasProvenance ann => HasProvenance (Arg binder var ann) where
  provenanceOf (Arg _ Explicit e) = provenanceOf e
  provenanceOf (Arg _ Implicit e) = expandProvenance (1, 1) (provenanceOf e)
  provenanceOf (Arg _ Instance e) = expandProvenance (2, 2) (provenanceOf e)

instance HasOwner ann => HasOwner (Arg binder var ann) where
  ownerOf (Arg ann _ _) = ownerOf ann

argExpr :: Arg binder var ann -> Expr binder var ann
argExpr (Arg _ _ e) = e

mapArgExpr :: (Expr binder1 var1 ann -> Expr binder2 var2 ann)
           -> Arg binder1 var1 ann -> Arg binder2 var2 ann
mapArgExpr f (Arg ann v e) = Arg ann v $ f e

replaceArgExpr :: Expr binder1 var1 ann -> Arg binder2 var2 ann -> Arg binder1 var1 ann
replaceArgExpr e = mapArgExpr (const e)

traverseArgExpr :: Monad m
                => (Expr binder1 var1 ann -> m (Expr binder2 var2 ann))
                -> Arg binder1 var1 ann
                -> m (Arg binder2 var2 ann)
traverseArgExpr f (Arg i v e) = Arg i v <$> f e

--------------------------------------------------------------------------------
-- Expressions

-- | Type of Vehicle Core expressions.
--
-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.
--
-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.
data Expr binder var ann

  -- | The type of types. The type @Type l@ has type @Type (l+1)@.
  = Type UniverseLevel

  -- | User annotation
  | Ann
    ann
    (Expr binder var ann)    -- The term
    (Expr binder var ann)    -- The type of the term

  -- | Application of one term to another.
  | App
    ann                              -- Annotation.
    (Expr binder var ann)            -- Function.
    (NonEmpty (Arg binder var ann)) -- Arguments.

  -- | Dependent product (subsumes both functions and universal quantification).
  | Pi
    ann                      -- Annotation.
    (Binder binder var ann)  -- The bound name
    (Expr   binder var ann)  -- (Dependent) result type.

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
    ann                      -- Annotation.
    (Expr   binder var ann)  -- Bound expression body.
    (Binder binder var ann)  -- Bound expression name.
    (Expr   binder var ann)  -- Expression body.

  -- | Lambda expressions (i.e. anonymous functions).
  | Lam
    ann                      -- Annotation.
    (Binder binder var ann)  -- Bound expression name.
    (Expr   binder var ann)  -- Expression body.

  -- | Built-in literal values e.g. numbers/booleans.
  | Literal
    ann               -- Annotation.
    Literal           -- Value.

  -- | A sequence of terms for e.g. list literals.
  | Seq
    ann               -- Annotation.
    [Expr binder var ann]    -- List of expressions.

  -- | A placeholder for a dictionary of builtin type-classes.
  -- At the moment doesn't carry around any meaningful information
  -- as we don't currently have user-defined type-classes. Later
  -- on they will carry around user definitions.
  | PrimDict (Expr binder var ann)

  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var, NFData ann) => NFData (Expr binder var ann)

instance HasProvenance ann => HasProvenance (Expr binder var ann) where
  provenanceOf (Hole p _) = p
  provenanceOf e          = provenanceOf (annotation e)

-- |Extract a term's annotation
annotation :: Expr binder var ann -> ann
annotation = \case
  Type     _         -> developerError "Should not be requesting an annotation from Type"
  Hole     _   _     -> developerError "Should not be requesting an annotation from Hole"
  PrimDict _         -> developerError "Should not be requesting an annotation from PrimitiveDict"
  Meta     ann _     -> ann
  Ann      ann _ _   -> ann
  App      ann _ _   -> ann
  Pi       ann _ _   -> ann
  Builtin  ann _     -> ann
  Var      ann _     -> ann
  Let      ann _ _ _ -> ann
  Lam      ann _ _   -> ann
  Literal  ann _     -> ann
  Seq      ann _     -> ann

--------------------------------------------------------------------------------
-- Identifiers

newtype Identifier = Identifier Symbol
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier s) = pretty s

instance NFData   Identifier
instance Hashable Identifier

class HasIdentifier a where
  identifierOf :: a -> Identifier

--------------------------------------------------------------------------------
-- Declarations

data DeclType
  = Network
  | Dataset

instance Pretty DeclType where
  pretty = \case
    Network  -> "network"
    Dataset  -> "dataset"

-- | Type of top-level declarations.
data Decl binder var ann
  = DeclNetw
    Provenance      -- Location in source file.
    Identifier      -- Network name.
    (Expr binder var ann)  -- Network type.
  | DeclData
    Provenance      -- Location in source file.
    Identifier      -- Dataset name.
    (Expr binder var ann)  -- Dataset type.
  | DefFun
    Provenance      -- Location in source file.
    Identifier      -- Bound function name.
    (Expr binder var ann)  -- Bound function type.
    (Expr binder var ann)  -- Bound function body.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var, NFData ann) => NFData (Decl binder var ann)

instance HasProvenance ann => HasProvenance (Decl binder var ann) where
  provenanceOf = \case
    DeclNetw p _ _ -> p
    DeclData p _ _ -> p
    DefFun p _ _ _ -> p

instance HasIdentifier (Decl binder var ann) where
  identifierOf = \case
    DeclNetw _ i _ -> i
    DeclData _ i _ -> i
    DefFun _ i _ _ -> i

--------------------------------------------------------------------------------
-- Programs

-- | Type of Vehicle Core programs.
newtype Prog binder var ann
  = Main [Decl binder var ann] -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var, NFData ann) => NFData (Prog binder var ann)

--------------------------------------------------------------------------------
-- Recursion principles

makeBaseFunctor ''Arg
makeBaseFunctor ''Binder
makeBaseFunctor ''Expr

--------------------------------------------------------------------------------
-- Type-classes

class HasType a where
  typeOf :: a binder var ann -> Expr binder var ann

instance HasType Binder where
  typeOf (Binder _ _ _ t) = t

instance HasType Decl where
  typeOf = \case
    DeclNetw _ _ t -> t
    DeclData _ _ t -> t
    DefFun _ _ t _ -> t

--------------------------------------------------------------------------------
-- Utilities

-- Preserves invariant that we never have two nested Apps
normApp :: Semigroup ann => ann -> Expr binder var ann -> NonEmpty (Arg binder var ann) -> Expr binder var ann
normApp p (App p' fun args') args = App (p' <> p) fun (args' <> args)
normApp p fun                args = App p fun args

normAppList :: Semigroup ann => ann -> Expr binder var ann -> [Arg binder var ann] -> Expr binder var ann
normAppList _   fun []           = fun
normAppList ann fun (arg : args) = normApp ann fun (arg :| args)