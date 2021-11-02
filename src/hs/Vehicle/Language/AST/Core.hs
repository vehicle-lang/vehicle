{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Language.AST.Core where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List.NonEmpty (NonEmpty(..))

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
    Owner           -- Has the binder been auto-inserted by the type-checker?
    Visibility      -- The visibility of the binder
    Name            -- The name of the bound variable
    (Expr var ann)  -- The (optional) type of the bound variable
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- At the moment explicit binders can only ever be provided by the user.
pattern ExplicitBinder :: Provenance -> Name -> Expr var ann -> Binder var ann
pattern ExplicitBinder p n t = Binder p TheUser Explicit n t

pattern UserImplicitBinder :: Provenance -> Name -> Expr var ann -> Binder var ann
pattern UserImplicitBinder p n t = Binder p TheUser Implicit n t

pattern MachineImplicitBinder :: Provenance -> Name -> Expr var ann -> Binder var ann
pattern MachineImplicitBinder p n t = Binder p TheMachine Implicit n t

pattern UserInstanceBinder :: Provenance -> Name -> Expr var ann -> Binder var ann
pattern UserInstanceBinder p n t = Binder p TheUser Instance n t

pattern MachineInstanceBinder :: Provenance -> Name -> Expr var ann -> Binder var ann
pattern MachineInstanceBinder p n t = Binder p TheMachine Instance n t

instance (NFData var, NFData ann) => NFData (Binder var ann)

instance HasProvenance (Binder var ann) where
  prov (Binder p _ _ _ _) = p

instance HasVisibility (Binder var ann) where
  vis (Binder _ _ v _ _) = v

instance HasOwner (Binder var ann) where
  getOwner (Binder _ owner _ _ _) = owner

-- |Extract the name of the bound variable
binderName :: Binder var ann -> Name
binderName (Binder _ _ _ name _) = name

-- |Extract the type of the bound variable
binderType :: Binder var ann -> Expr var ann
binderType (Binder _ _ _ _ t) = t

mapBinderType :: (Expr var1 ann1 -> Expr var2 ann2) -> Binder var1 ann1 -> Binder var2 ann2
mapBinderType f (Binder p i v n e) = Binder p i v n $ f e

replaceBinderType :: Expr var1 ann1 -> Binder var2 ann2 -> Binder var1 ann1
replaceBinderType e = mapBinderType (const e)

traverseBinderType :: Monad m
                   => (Expr var1 ann1 -> m (Expr var2 ann2))
                   -> Binder var1 ann1
                   -> m (Binder var2 ann2)
traverseBinderType f (Binder p i v n e) = Binder p i v n <$> f e

--------------------------------------------------------------------------------
-- Function arguments

data Arg var ann
  = Arg
    Owner           -- Has the argument been auto-inserted by the type-checker?
    Visibility      -- The visibility of the argument
    (Expr var ann)  -- The argument expression
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

-- At the moment explicit arguments can only ever be provided by the user.
pattern ExplicitArg :: Expr var ann -> Arg var ann
pattern ExplicitArg e = Arg TheUser Explicit e

pattern UserImplicitArg :: Expr var ann -> Arg var ann
pattern UserImplicitArg e = Arg TheUser Implicit e

pattern MachineImplicitArg :: Expr var ann -> Arg var ann
pattern MachineImplicitArg e = Arg TheMachine Implicit e

pattern UserInstanceArg :: Expr var ann -> Arg var ann
pattern UserInstanceArg e = Arg TheUser Instance e

pattern MachineInstanceArg :: Expr var ann -> Arg var ann
pattern MachineInstanceArg e = Arg TheMachine Instance e

instance (NFData var, NFData ann) => NFData (Arg var ann)

instance HasVisibility (Arg var ann) where
  vis (Arg _ v _) = v

instance HasProvenance ann => HasProvenance (Arg var ann) where
  prov (Arg _ Explicit e) = prov e
  prov (Arg _ Implicit e) = expandProvenance (1, 1) (prov e)
  prov (Arg _ Instance e) = expandProvenance (2, 2) (prov e)

instance HasOwner (Arg var ann) where
  getOwner (Arg owner _ _) = owner

argExpr :: Arg var ann -> Expr var ann
argExpr (Arg _ _ e) = e

mapArgExpr :: (Expr var1 ann1 -> Expr var2 ann2) -> Arg var1 ann1 -> Arg var2 ann2
mapArgExpr f (Arg i v e) = Arg i v $ f e

replaceArgExpr :: Expr var1 ann1 -> Arg var2 ann2 -> Arg var1 ann1
replaceArgExpr e = mapArgExpr (const e)

traverseArgExpr :: Monad m
                => (Expr var1 ann1 -> m (Expr var2 ann2))
                -> Arg var1 ann1
                -> m (Arg var2 ann2)
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

instance HasProvenance ann => HasProvenance (Expr var ann) where
  prov (Hole p _) = p
  prov e          = prov (annotation e)

-- |Extract a term's annotation
annotation :: Expr name ann -> ann
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
