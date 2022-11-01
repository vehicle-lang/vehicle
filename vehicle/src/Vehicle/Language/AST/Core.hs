{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Language.AST.Core where

import Control.DeepSeq (NFData)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Generics (Generic)

import Vehicle.Language.AST.Builtin (Builtin, Linearity (..), Polarity (..))
import Vehicle.Language.AST.Provenance
import Vehicle.Language.AST.Relevance
import Vehicle.Language.AST.Visibility
import Vehicle.Prelude
import Vehicle.Resource (ResourceType)

--------------------------------------------------------------------------------
-- Universes

type UniverseLevel = Int

data Universe
  = TypeUniv UniverseLevel
  | LinearityUniv
  | PolarityUniv
  deriving (Eq, Ord, Show, Generic)

instance NFData   Universe
instance Hashable Universe

instance Pretty Universe where
  pretty = \case
    TypeUniv l    -> "Type" <+> pretty l
    LinearityUniv -> "LinearityUniverse"
    PolarityUniv  -> "PolarityUniverse"

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
data Literal
  = LUnit
  | LBool  Bool
  | LIndex Int Int
  | LNat   Int
  | LInt   Int
  | LRat   Rational
  deriving (Eq, Ord, Show, Generic)

instance NFData   Literal
instance Hashable Literal

instance Pretty Literal where
  pretty = \case
    LUnit      -> "()"
    LBool  x   -> pretty x
    LIndex _ x -> pretty x
    LNat   x   -> pretty x
    LInt   x   -> pretty x
    LRat   x   -> pretty x

--------------------------------------------------------------------------------
-- Binders

-- | Binder for lambda and let expressions
--
-- The binder stores the optional type annotation in order to ensure
-- reversibility during delaboration, and that as the type annotation was
-- manually provided by the user it never needs to be updated after unification
-- and type-class resolution.
data GenericBinder binder expr = Binder
  { binderProvenance     :: Provenance
  , binderVisibility     :: Visibility
  -- ^ The visibility of the binder
  , binderRelevance      :: Relevance
  -- ^ The relevancy of the binder
  , binderRepresentation :: binder
  -- ^ The representation of the bound variable
  , binderType           :: expr
  -- The type of the bound variable
  } deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

pattern ExplicitBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern ExplicitBinder p n t = Binder p Explicit Relevant n t

pattern ImplicitBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern ImplicitBinder p n t = Binder p Implicit Relevant n t

pattern InstanceBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern InstanceBinder p n t = Binder p Instance Relevant  n t

pattern IrrelevantInstanceBinder :: Provenance -> binder -> expr -> GenericBinder binder expr
pattern IrrelevantInstanceBinder p n t = Binder p Instance Irrelevant  n t

instance (NFData binder, NFData expr) => NFData (GenericBinder binder expr)

instance HasProvenance (GenericBinder binder expr) where
  provenanceOf = binderProvenance

instance HasVisibility (GenericBinder binder expr) where
  visibilityOf = binderVisibility

instance HasRelevance (GenericBinder binder expr) where
  relevanceOf = binderRelevance

pairBinder :: (GenericBinder binder a, b) -> GenericBinder binder (a, b)
pairBinder (Binder p v r b x, y) = Binder p v r b (x, y)

unpairBinder :: GenericBinder binder (a, b) -> (GenericBinder binder a, b)
unpairBinder (Binder p v r b (x, y)) = (Binder p v r b x, y)

unpairBinderRep :: GenericBinder (a, b) e -> (GenericBinder a e, b)
unpairBinderRep (Binder p v r (x, y) t) = (Binder p v r x t, y)

mapBinderRep :: (a -> b) -> GenericBinder a e -> GenericBinder b e
mapBinderRep f (Binder p v r b t) = Binder p v r (f b) t

replaceBinderRep :: b -> GenericBinder a e -> GenericBinder b e
replaceBinderRep b' (Binder p v r _b t) = Binder p v r b' t

replaceBinderType :: expr1
                  -> GenericBinder binder expr2
                  -> GenericBinder binder expr1
replaceBinderType e = fmap (const e)

type Binder binder var = GenericBinder binder (Expr binder var)

-- | This horrible construction is needed because |Binder| is a type synonym
-- synonyms which can't be used as functions at the type level. This wraps
-- it as required.
newtype Binder' var binder = WrapBinder
  { unwrapBinder :: Binder var binder
  }

--------------------------------------------------------------------------------
-- Function arguments

-- | An argument to a function, parameterised by the type of expression it
-- stores.
data GenericArg expr = Arg
  { argProvenance :: Provenance
    -- ^ Has the argument been auto-inserted by the type-checker?
  , argVisibility :: Visibility
    -- ^ The visibility of the argument
  , argRelevance  :: Relevance
    -- ^ The relevancy of the argument
  , argExpr       :: expr
    -- ^ The argument expression
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData expr) => NFData (GenericArg expr)

instance HasProvenance (GenericArg expr) where
  provenanceOf = argProvenance

instance HasVisibility (GenericArg expr) where
  visibilityOf = argVisibility

instance HasRelevance (GenericArg expr) where
  relevanceOf = argRelevance

pattern ExplicitArg :: Provenance -> expr -> GenericArg expr
pattern ExplicitArg p e = Arg p Explicit Relevant e

pattern ImplicitArg :: Provenance -> expr -> GenericArg expr
pattern ImplicitArg p e = Arg p Implicit Relevant e

pattern IrrelevantImplicitArg :: Provenance -> expr -> GenericArg expr
pattern IrrelevantImplicitArg p e = Arg p Implicit Irrelevant e

pattern InstanceArg :: Provenance -> expr -> GenericArg expr
pattern InstanceArg p e = Arg p Instance Relevant e

pattern IrrelevantInstanceArg :: Provenance -> expr -> GenericArg expr
pattern IrrelevantInstanceArg p e = Arg p Instance Irrelevant e

pairArg :: (GenericArg a, b) -> GenericArg (a, b)
pairArg (Arg p v r x, y) = Arg p v r (x, y)

unpairArg :: GenericArg (a, b) -> (GenericArg a, b)
unpairArg (Arg p v r (x, y)) = (Arg p v r x, y)

replaceArgExpr :: expr1 -> GenericArg expr2 -> GenericArg expr1
replaceArgExpr e = fmap (const e)

traverseExplicitArgExpr :: Monad m
                        => (expr -> m expr)
                        -> GenericArg expr
                        -> m (GenericArg expr)
traverseExplicitArgExpr f (ExplicitArg i e) = ExplicitArg i <$> f e
traverseExplicitArgExpr _ arg               = return arg


type Arg binder var = GenericArg (Expr binder var)

-- | This horrible construction is needed because |Arg| is a type synonym
-- synonyms which can't be used as functions at the type level. This wraps
-- it as required.
newtype Arg' var binder = WrapArg
  { unwrapArg :: Arg var binder
  }

--------------------------------------------------------------------------------
-- Expressions

-- | Type of Vehicle internal expressions.
--
-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.
--
-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.
data Expr binder var

  -- | A universe, used to type types.
  = Universe
    Provenance
    Universe

  -- | User annotation
  | Ann
    Provenance
    (Expr binder var)    -- The term
    (Expr binder var)    -- The type of the term

  -- | Application of one term to another.
  | App
    Provenance
    (Expr binder var)           -- Function.
    (NonEmpty (Arg binder var)) -- Arguments.

  -- | Dependent product (subsumes both functions and universal quantification).
  | Pi
    Provenance
    (Binder binder var)  -- The bound name
    (Expr   binder var)  -- (Dependent) result type.

  -- | Terms consisting of constants that are built into the language.
  | Builtin
    Provenance
    Builtin          -- Builtin name.

  -- | Variables that are bound by other expressions
  | Var
    Provenance
    var              -- Variable name.

  -- | A hole in the program.
  | Hole
    Provenance
    Name             -- Hole name.

  -- | Unsolved meta variables.
  | Meta
    Provenance
    Meta             -- Meta variable number.

  -- | Let expressions. We have these in the core syntax because we want to
  -- cross compile them to various backends.
  --
  -- NOTE: that the order of the bound expression and the binder is reversed
  -- to better mimic the flow of the context, which makes writing monadic
  -- operations concisely much easier.
  | Let
    Provenance
    (Expr   binder var)  -- Bound expression body.
    (Binder binder var)  -- Bound expression name.
    (Expr   binder var)  -- Expression body.

  -- | Lambda expressions (i.e. anonymous functions).
  | Lam
    Provenance
    (Binder binder var)  -- Bound expression name.
    (Expr   binder var)  -- Expression body.

  -- | Built-in literal values e.g. numbers/booleans.
  | Literal
    Provenance
    Literal                  -- Value.

  -- | A sequence of terms for e.g. list literals.
  | LVec
    Provenance
    [Expr binder var]    -- List of expressions.

  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var) => NFData (Expr binder var)

type Type = Expr

instance HasProvenance (Expr binder var) where
  provenanceOf = \case
    Universe p _     -> p
    Hole     p _     -> p
    Meta     p _     -> p
    Ann      p _ _   -> p
    App      p _ _   -> p
    Pi       p _ _   -> p
    Builtin  p _     -> p
    Var      p _     -> p
    Let      p _ _ _ -> p
    Lam      p _ _   -> p
    Literal  p _     -> p
    LVec     p _     -> p

--------------------------------------------------------------------------------
-- Identifiers

newtype Identifier = Identifier Name
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier s) = pretty s

instance NFData   Identifier
instance Hashable Identifier

class HasIdentifier a where
  identifierOf :: a -> Identifier

--------------------------------------------------------------------------------
-- Property annotations

-- | A marker for how a declaration is used as part of a quantified property
-- and therefore needs to be lifted to the type-level when being exported, or
-- whether it is only used unquantified and therefore needs to be computable.
data PropertyInfo
  = PropertyInfo Linearity Polarity
  deriving (Show, Eq, Generic)

instance NFData PropertyInfo

instance Pretty PropertyInfo where
  pretty (PropertyInfo lin pol) = pretty lin <+> pretty pol

--------------------------------------------------------------------------------
-- Declarations

-- | Type of top-level declarations.
data GenericDecl expr
  = DefResource
    Provenance             -- Location in source file.
    ResourceType           -- Type of resource.
    Identifier             -- Name of resource.
    expr                   -- Vehicle type of the resource.

  | DefFunction
    Provenance             -- Location in source file.
    Identifier             -- Bound function name.
    expr                   -- Bound function type.
    expr                   -- Bound function body.

  | DefPostulate
    Provenance
    Identifier
    expr
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance NFData expr => NFData (GenericDecl expr)

instance HasProvenance (GenericDecl expr) where
  provenanceOf = \case
    DefResource p _ _ _  -> p
    DefFunction p _  _ _ -> p
    DefPostulate p _ _   -> p

instance HasIdentifier (GenericDecl expr) where
  identifierOf = \case
    DefResource  _ _ i _  -> i
    DefFunction  _  i _ _ -> i
    DefPostulate _ i _    -> i

bodyOf :: GenericDecl expr -> Maybe expr
bodyOf = \case
  DefResource{}       -> Nothing
  DefFunction _ _ _ e -> Just e
  DefPostulate{}      -> Nothing

type Decl binder var = GenericDecl (Expr binder var)

-- | This horrible construction is needed because |Decl| is a type synonym
-- synonyms which can't be used as functions at the type level. This wraps
-- it as required.
newtype Decl' var binder = WrapDecl
  { unwrapDecl :: Decl var binder
  }

--------------------------------------------------------------------------------
-- Programs

-- | Type of Vehicle internal programs.
newtype GenericProg expr
  = Main [GenericDecl expr] -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance NFData expr => NFData (GenericProg expr)

traverseDecls :: Monad m
              => (Decl binder1 var1 -> m (Decl binder2 var2))
              -> Prog binder1 var1
              -> m (Prog binder2 var2)
traverseDecls f (Main ds) = Main <$> traverse f ds

type Prog binder var = GenericProg (Expr binder var)

-- | This horrible construction is needed because |Prog| is a type synonym
-- synonyms which can't be used as functions at the type level. This wraps
-- it as required.
newtype Prog' var binder = WrapProg
  { unwrapProg :: Prog var binder
  }

--------------------------------------------------------------------------------
-- Recursion principles

makeBaseFunctor ''GenericArg
makeBaseFunctor ''GenericBinder
makeBaseFunctor ''Expr

--------------------------------------------------------------------------------
-- Type-classes

typeOf :: GenericDecl expr -> expr
typeOf = \case
  DefResource _ _ _ t -> t
  DefFunction _ _ t _ -> t
  DefPostulate _ _ t  -> t

typeOf' :: GenericBinder binder expr -> expr
typeOf' = binderType

--------------------------------------------------------------------------------
-- Utilities

-- Preserves invariant that we never have two nested Apps
normApp :: Provenance -> Expr binder var -> NonEmpty (Arg binder var) -> Expr binder var
normApp p (App p' fun args') args = App (p' <> p) fun (args' <> args)
normApp p fun                args = App p fun args

normAppList :: Provenance -> Expr binder var -> [Arg binder var] -> Expr binder var
normAppList _   fun []           = fun
normAppList ann fun (arg : args) = normApp ann fun (arg :| args)
