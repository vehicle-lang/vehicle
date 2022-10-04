{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Language.AST.Core where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Hashable (Hashable)

import Vehicle.Prelude
import Vehicle.Resource (ResourceType)
import Vehicle.Language.AST.Builtin(Builtin, Linearity (..), Polarity (..))
import Vehicle.Language.AST.Visibility
import Vehicle.Language.AST.Provenance
import Vehicle.Language.AST.Relevance

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
data Binder binder var
  = Binder
    Provenance
    Visibility         -- The visibility of the binder
    Relevance          -- The relevancy of the binder
    binder             -- The representation of the bound variable
    (Expr binder var)  -- The (optional) type of the bound variable
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

pattern ExplicitBinder :: Provenance -> binder -> Expr binder var -> Binder binder var
pattern ExplicitBinder p n t = Binder p Explicit Relevant n t

pattern ImplicitBinder :: Provenance -> binder -> Expr binder var -> Binder binder var
pattern ImplicitBinder p n t = Binder p Implicit Relevant n t

pattern InstanceBinder :: Provenance -> binder -> Expr binder var -> Binder binder var
pattern InstanceBinder p n t = Binder p Instance Relevant  n t

pattern IrrelevantInstanceBinder :: Provenance -> binder -> Expr binder var -> Binder binder var
pattern IrrelevantInstanceBinder p n t = Binder p Instance Irrelevant  n t

instance (NFData binder, NFData var) => NFData (Binder binder var)

instance HasProvenance (Binder binder var) where
  provenanceOf (Binder p _ _ _ _) = p

instance HasVisibility (Binder binder var) where
  visibilityOf (Binder _ v _ _ _) = v

instance HasRelevance (Binder binder var) where
  relevanceOf (Binder _ _ r _ _) = r

mapBinderType :: (Expr binder var1 -> Expr binder var2)
              -> Binder binder var1 -> Binder binder var2
mapBinderType f (Binder ann v r n e) = Binder ann v r n $ f e

replaceBinderType :: Expr binder var1
                  -> Binder binder var2
                  -> Binder binder var1
replaceBinderType e = mapBinderType (const e)

traverseBinderType :: Monad m
                   => (Expr binder var1 -> m (Expr binder var2))
                   -> Binder binder var1
                   -> m (Binder binder var2)
traverseBinderType f (Binder ann v r n e) = Binder ann v r n <$> f e

--------------------------------------------------------------------------------
-- Function arguments

data Arg binder var
  = Arg
    Provenance         -- Has the argument been auto-inserted by the type-checker?
    Visibility         -- The visibility of the argument
    Relevance          -- The relevancy of the argument
    (Expr binder var)  -- The argument expression
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

pattern ExplicitArg :: Provenance -> Expr binder var -> Arg binder var
pattern ExplicitArg p e = Arg p Explicit Relevant e

pattern ImplicitArg :: Provenance -> Expr binder var -> Arg binder var
pattern ImplicitArg p e = Arg p Implicit Relevant e

pattern IrrelevantImplicitArg :: Provenance -> Expr binder var -> Arg binder var
pattern IrrelevantImplicitArg p e = Arg p Implicit Irrelevant e

pattern InstanceArg :: Provenance -> Expr binder var -> Arg binder var
pattern InstanceArg p e = Arg p Instance Relevant e

pattern IrrelevantInstanceArg :: Provenance -> Expr binder var -> Arg binder var
pattern IrrelevantInstanceArg p e = Arg p Instance Irrelevant e

instance (NFData binder, NFData var) => NFData (Arg binder var)

instance HasProvenance (Arg binder var) where
  provenanceOf (Arg p _ _ _) = p

instance HasVisibility (Arg binder var) where
  visibilityOf (Arg _ v _ _) = v

instance HasRelevance (Arg binder var) where
  relevanceOf (Arg _ _ r _) = r

argExpr :: Arg binder var -> Expr binder var
argExpr (Arg _ _ _ e) = e

mapArgExpr :: (Expr binder1 var1 -> Expr binder2 var2)
           -> Arg binder1 var1 -> Arg binder2 var2
mapArgExpr f (Arg ann v r e) = Arg ann v r $ f e

replaceArgExpr :: Expr binder1 var1 -> Arg binder2 var2 -> Arg binder1 var1
replaceArgExpr e = mapArgExpr (const e)

traverseArgExpr :: Monad m
                => (Expr binder1 var1 -> m (Expr binder2 var2))
                -> Arg binder1 var1
                -> m (Arg binder2 var2)
traverseArgExpr f (Arg i v r e) = Arg i v r <$> f e

traverseExplicitArgExpr :: Monad m
                        => (Expr binder var -> m (Expr binder var))
                        -> Arg binder var
                        -> m (Arg binder var)
traverseExplicitArgExpr f (ExplicitArg i e) = ExplicitArg i <$> f e
traverseExplicitArgExpr _ arg               = return arg

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
    Symbol           -- Hole name.

  -- | Unsolved meta variables.
  | Meta
    Provenance
    Meta             -- Meta variable number.

  -- | Let expressions.
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

newtype Identifier = Identifier Symbol
  deriving (Eq, Ord, Show, Generic)

instance Pretty Identifier where
  pretty (Identifier s) = pretty s

instance NFData   Identifier
instance Hashable Identifier

class HasIdentifier a where
  identifierOf :: a -> Identifier

symbolOf :: Identifier -> Symbol
symbolOf (Identifier s) = s

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
data Decl binder var
  = DefResource
    Provenance             -- Location in source file.
    ResourceType           -- Type of resource.
    Identifier             -- Name of resource.
    (Expr binder var)      -- Vehicle type of the resource.

  | DefFunction
    Provenance             -- Location in source file.
    Identifier             -- Bound function name.
    (Expr binder var)      -- Bound function type.
    (Expr binder var)      -- Bound function body.

  | DefPostulate
    Provenance
    Identifier
    (Expr binder var)
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var) => NFData (Decl binder var)

instance HasProvenance (Decl binder var) where
  provenanceOf = \case
    DefResource p _ _ _  -> p
    DefFunction p _  _ _ -> p
    DefPostulate p _ _   -> p

instance HasIdentifier (Decl binder var) where
  identifierOf = \case
    DefResource  _ _ i _  -> i
    DefFunction  _  i _ _ -> i
    DefPostulate _ i _    -> i

mapDeclExprs :: (Expr binder1 var1 -> Expr binder2 var2)
             -> Decl binder1 var1
             -> Decl binder2 var2
mapDeclExprs f = \case
  DefResource p r n t -> DefResource p r n (f t)
  DefFunction p n t e -> DefFunction p n (f t) (f e)
  DefPostulate p n t  -> DefPostulate p n (f t)

traverseDeclExprs :: Monad m
                  => (Expr binder1 var1 -> m (Expr binder2 var2))
                  -> Decl binder1 var1
                  -> m (Decl binder2 var2)
traverseDeclExprs f = \case
  DefResource p r n t -> DefResource p r n <$> f t
  DefFunction p n t e -> DefFunction p n <$> f t <*> f e
  DefPostulate p n t  -> DefPostulate p n <$> f t

bodyOf :: Decl binder var -> Maybe (Expr binder var)
bodyOf = \case
  DefResource{}       -> Nothing
  DefFunction _ _ _ e -> Just e
  DefPostulate{}      -> Nothing

--------------------------------------------------------------------------------
-- Programs

-- | Type of Vehicle internal programs.
newtype Prog binder var
  = Main [Decl binder var] -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var) => NFData (Prog binder var)

traverseProg :: Monad m
             => (Decl binder1 var1 -> m (Decl binder2 var2))
             -> Prog binder1 var1
             -> m (Prog binder2 var2)
traverseProg f (Main ds) = Main <$> traverse f ds

--------------------------------------------------------------------------------
-- Recursion principles

makeBaseFunctor ''Arg
makeBaseFunctor ''Binder
makeBaseFunctor ''Expr

--------------------------------------------------------------------------------
-- Type-classes

class HasType a where
  typeOf :: a binder var -> Expr binder var

instance HasType Binder where
  typeOf (Binder _ _ _ _ t) = t

instance HasType Decl where
  typeOf = \case
    DefResource _ _ _ t -> t
    DefFunction _ _ t _ -> t
    DefPostulate _ _ t  -> t

--------------------------------------------------------------------------------
-- Utilities

-- Preserves invariant that we never have two nested Apps
normApp :: Provenance -> Expr binder var -> NonEmpty (Arg binder var) -> Expr binder var
normApp p (App p' fun args') args = App (p' <> p) fun (args' <> args)
normApp p fun                args = App p fun args

normAppList :: Provenance -> Expr binder var -> [Arg binder var] -> Expr binder var
normAppList _   fun []           = fun
normAppList ann fun (arg : args) = normApp ann fun (arg :| args)