{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Language.AST.Core where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Hashable (Hashable)
import Data.Maybe (isJust)

import Vehicle.Prelude
import Vehicle.Resource (ResourceType)
import Vehicle.Language.AST.Builtin (Builtin, Polarity(..))
import Vehicle.Language.AST.Visibility
import Vehicle.Language.AST.Provenance

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
data Binder binder var
  = Binder
    Provenance
    Visibility         -- The visibility of the binder
    binder             -- The representation of the bound variable
    (Expr binder var)  -- The (optional) type of the bound variable
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

pattern ExplicitBinder :: Provenance -> binder -> Expr binder var -> Binder binder var
pattern ExplicitBinder p n t = Binder p Explicit n t

pattern ImplicitBinder :: Provenance -> binder -> Expr binder var -> Binder binder var
pattern ImplicitBinder p n t = Binder p Implicit n t

pattern InstanceBinder :: Provenance -> binder -> Expr binder var -> Binder binder var
pattern InstanceBinder p n t = Binder p Instance n t

instance (NFData binder, NFData var) => NFData (Binder binder var)

instance HasVisibility (Binder binder var) where
  visibilityOf (Binder _ visibility _ _) = visibility

mapBinderType :: (Expr binder var1 -> Expr binder var2)
              -> Binder binder var1 -> Binder binder var2
mapBinderType f (Binder ann v n e) = Binder ann v n $ f e

replaceBinderType :: Expr binder var1
                  -> Binder binder var2
                  -> Binder binder var1
replaceBinderType e = mapBinderType (const e)

traverseBinderType :: Monad m
                   => (Expr binder var1 -> m (Expr binder var2))
                   -> Binder binder var1
                   -> m (Binder binder var2)
traverseBinderType f (Binder ann v n e) = Binder ann v n <$> f e

--------------------------------------------------------------------------------
-- Function arguments

data Arg binder var
  = Arg
    Provenance         -- Has the argument been auto-inserted by the type-checker?
    Visibility         -- The visibility of the argument
    (Expr binder var)  -- The argument expression
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

-- At the moment explicit arguments can only ever be provided by the user.
pattern ExplicitArg :: Provenance -> Expr binder var -> Arg binder var
pattern ExplicitArg ann e = Arg ann Explicit e

pattern ImplicitArg :: Provenance -> Expr binder var -> Arg binder var
pattern ImplicitArg ann e = Arg ann Implicit e

pattern InstanceArg :: Provenance -> Expr binder var -> Arg binder var
pattern InstanceArg ann e = Arg ann Instance e

instance (NFData binder, NFData var) => NFData (Arg binder var)

instance HasVisibility (Arg binder var) where
  visibilityOf (Arg _ v _) = v

argExpr :: Arg binder var -> Expr binder var
argExpr (Arg _ _ e) = e

mapArgExpr :: (Expr binder1 var1 -> Expr binder2 var2)
           -> Arg binder1 var1 -> Arg binder2 var2
mapArgExpr f (Arg ann v e) = Arg ann v $ f e

replaceArgExpr :: Expr binder1 var1 -> Arg binder2 var2 -> Arg binder1 var1
replaceArgExpr e = mapArgExpr (const e)

traverseArgExpr :: Monad m
                => (Expr binder1 var1 -> m (Expr binder2 var2))
                -> Arg binder1 var1
                -> m (Arg binder2 var2)
traverseArgExpr f (Arg i v e) = Arg i v <$> f e

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

  -- | The type of types. The type @Type l@ has type @Type (l+1)@.
  = Type
    Provenance
    UniverseLevel

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
  | LSeq
    Provenance
    (Expr binder var)    -- Type-class dictionary.
    [Expr binder var]    -- List of expressions.

  -- | A placeholder for a dictionary of builtin type-classes.
  -- At the moment doesn't carry around any meaningful information
  -- as we don't currently have user-defined type-classes. Later
  -- on they will carry around user definitions.
  | PrimDict
    Provenance
    (Expr binder var)

  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var) => NFData (Expr binder var)

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
-- Property annotations

-- | A marker for how a declaration is used as part of a quantified property
-- and therefore needs to be lifted to the type-level when being exported, or
-- whether it is only used unquantified and therefore needs to be computable.
newtype PropertyInfo
  = PropertyInfo Polarity
  deriving (Show, Eq, Generic)

instance NFData PropertyInfo

isProperty :: Maybe PropertyInfo -> Bool
isProperty = isJust

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
    (Maybe PropertyInfo)   -- Auxiliary typing information about a property.
    Identifier             -- Bound function name.
    (Expr binder var)      -- Bound function type.
    (Expr binder var)      -- Bound function body.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var) => NFData (Decl binder var)

instance HasIdentifier (Decl binder var) where
  identifierOf = \case
    DefResource _ _ i _   -> i
    DefFunction _ _ i _ _ -> i

traverseDeclExprs :: Monad m
                  => (Expr binder1 var1 -> m (Expr binder2 var2))
                  -> Decl binder1 var1
                  -> m (Decl binder2 var2)
traverseDeclExprs f (DefResource ann r n t)   = DefResource ann r n <$> f t
traverseDeclExprs f (DefFunction ann u n t e) = DefFunction ann u n <$> f t <*> f e

bodyOf :: Decl binder var -> Maybe (Expr binder var)
bodyOf DefResource{}           = Nothing
bodyOf (DefFunction _ _ _ _ e) = Just e

--------------------------------------------------------------------------------
-- Programs

-- | Type of Vehicle internal programs.
newtype Prog binder var
  = Main [Decl binder var] -- ^ List of declarations.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var) => NFData (Prog binder var)

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
  typeOf (Binder _ _ _ t) = t

instance HasType Decl where
  typeOf = \case
    DefResource _ _ _ t   -> t
    DefFunction _ _ _ t _ -> t

--------------------------------------------------------------------------------
-- Annotations

instance HasProvenance (Binder binder var) where
  provenanceOf (Binder ann _ _ _) = ann

instance HasProvenance (Arg binder var) where
  provenanceOf (Arg ann _ _) = ann

instance HasProvenance (Expr binder var) where
  provenanceOf = \case
    Type     ann _     -> ann
    PrimDict ann _     -> ann
    Hole     ann _     -> ann
    Meta     ann _     -> ann
    Ann      ann _ _   -> ann
    App      ann _ _   -> ann
    Pi       ann _ _   -> ann
    Builtin  ann _     -> ann
    Var      ann _     -> ann
    Let      ann _ _ _ -> ann
    Lam      ann _ _   -> ann
    Literal  ann _     -> ann
    LSeq     ann _ _   -> ann

instance HasProvenance (Decl binder var) where
  provenanceOf = \case
    DefResource ann _ _ _   -> ann
    DefFunction ann _ _ _ _ -> ann

--------------------------------------------------------------------------------
-- Utilities

-- Preserves invariant that we never have two nested Apps
normApp :: Provenance -> Expr binder var -> NonEmpty (Arg binder var) -> Expr binder var
normApp p (App p' fun args') args = App (p' <> p) fun (args' <> args)
normApp p fun                args = App p fun args

normAppList :: Provenance -> Expr binder var -> [Arg binder var] -> Expr binder var
normAppList _   fun []           = fun
normAppList ann fun (arg : args) = normApp ann fun (arg :| args)