module Vehicle.Syntax.AST.Expr
  ( -- * Generic expressions
    Arg,
    Binder,
    Expr
      ( Universe,
        App,
        Pi,
        Builtin,
        BoundVar,
        FreeVar,
        Hole,
        Meta,
        Let,
        Lam,
        Record
      ),
    Type,
    UniverseLevel (..),
    Telescope,

    -- * Utilities
    isTypeSynonym,
    isPi,
    mkHole,
    normAppList,
    getFreeVarApp,
    getBuiltinApp,
    pattern TypeUniverse,
    pattern BuiltinExpr,
  )
where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Syntax.AST.Arg
import Vehicle.Syntax.AST.Binder
import Vehicle.Syntax.AST.Meta (MetaID)
import Vehicle.Syntax.AST.Name (Identifier, Name)
import Vehicle.Syntax.AST.Provenance (HasProvenance (..), Provenance, fillInProvenance)
import Vehicle.Syntax.Prelude ()

--------------------------------------------------------------------------------
-- Universes

newtype UniverseLevel = UniverseLevel Int
  deriving (Eq, Ord, Show, Generic)

instance NFData UniverseLevel

instance Hashable UniverseLevel

instance Serialize UniverseLevel

instance Pretty UniverseLevel where
  pretty (UniverseLevel l) = "Type" <+> pretty l

--------------------------------------------------------------------------------
-- Expressions

-- | Type of Vehicle internal expressions.
--
-- Annotations are parameterised over so that they can
-- store arbitrary information used in e.g. type-checking.
--
-- Names are parameterised over so that they can store
-- either the user assigned names or deBruijn indices.
data Expr var builtin
  = -- | A universe, used to type types.
    Universe
      Provenance
      UniverseLevel
  | -- | Application of one term to another. Doesn't have provenance as it has no syntax in the grammar.
    UnsafeApp
      (Expr var builtin) -- Function.
      (NonEmpty (Arg var builtin)) -- Arguments.
  | -- | Dependent product (subsumes both functions and universal quantification).
    Pi
      Provenance
      (Binder var builtin) -- The bound name
      (Expr var builtin) -- (Dependent) result type.
  | -- | Terms consisting of constants that are built into the language.
    Builtin
      Provenance
      builtin -- Builtin name.
  | -- | Variables that are bound locally by other expressions
    BoundVar
      Provenance
      var -- Variable name.
  | -- | Variables that refer to other declarations
    FreeVar
      Provenance
      Identifier -- Declaration name
  | -- | A hole in the program.
    Hole
      Provenance
      Name -- Hole name.
  | -- | Unsolved meta variables.
    Meta
      Provenance
      MetaID -- Meta variable number.
  | -- | Records
    Record
      Provenance
      [ (Name, Expr var builtin)]
  | -- | Let expressions. We have these in the core syntax because we want to
    -- cross compile them to various backends.
    --
    -- NOTE: that the order of the bound expression and the binder is reversed
    -- to better mimic the flow of the context, which makes writing monadic
    -- operations concisely much easier.
    Let
      Provenance
      (Expr var builtin) -- Bound expression body.
      (Binder var builtin) -- Bound expression name.
      (Expr var builtin) -- Expression body.
  | -- | Lambda expressions (i.e. anonymous functions).
    Lam
      Provenance
      (Binder var builtin) -- Bound expression name.
      (Expr var builtin) -- Expression body.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

--------------------------------------------------------------------------------
-- Safe applications

-- | Smart constructor for applications with possibly no arguments.
normAppList :: Expr var builtin -> [Arg var builtin] -> Expr var builtin
normAppList f [] = f
normAppList f (x : xs) = App f (x :| xs)

-- | Smart constructor for applications.
normApp :: Expr var builtin -> NonEmpty (Arg var builtin) -> Expr var builtin
normApp (UnsafeApp f xs) ys = UnsafeApp f (xs <> ys)
normApp f xs = UnsafeApp f xs

-- | Safe pattern synonym for applications.
pattern App :: Expr var builtin -> NonEmpty (Arg var builtin) -> Expr var builtin
pattern App f xs <- UnsafeApp f xs
  where
    App f xs = normApp f xs

{-# COMPLETE Universe, App, Pi, Builtin, BoundVar, FreeVar, Hole, Meta, Let, Lam #-}

--------------------------------------------------------------------------------
-- Instances

instance (NFData var, NFData builtin) => NFData (Expr var builtin)

instance (Serialize var, Serialize builtin) => Serialize (Expr var builtin)

instance HasProvenance (Expr var builtin) where
  provenanceOf = \case
    Universe p _ -> p
    Hole p _ -> p
    Meta p _ -> p
    App e xs -> fillInProvenance (provenanceOf e :| provenanceOf xs : [])
    Pi p _ _ -> p
    Builtin p _ -> p
    BoundVar p _ -> p
    FreeVar p _ -> p
    Let p _ _ _ -> p
    Lam p _ _ -> p

--------------------------------------------------------------------------------
-- The AST datatypes specialised to the Expr type

type Type = Expr

type Binder var builtin = GenericBinder (Expr var builtin)

type Arg var builtin = GenericArg (Expr var builtin)

type Telescope var builtin = [Binder var builtin]

--------------------------------------------------------------------------------
-- Utilities

mkHole :: Provenance -> Name -> Expr var builtin
mkHole p name = Hole p ("_" <> name)

-- | Tests if a definition's type indicates that the definition is a type
-- synonym.
isTypeSynonym :: Type var builtin -> Bool
isTypeSynonym = \case
  Universe {} -> True
  Pi _ _ res -> isTypeSynonym res
  _ -> False

isPi :: Type var builtin -> Bool
isPi Pi {} = True
isPi _ = False

pattern TypeUniverse :: Provenance -> Int -> Expr var builtin
pattern TypeUniverse p l = Universe p (UniverseLevel l)

pattern BuiltinExpr ::
  Provenance ->
  builtin ->
  NonEmpty (Arg var builtin) ->
  Expr var builtin
pattern BuiltinExpr p b args <- App (Builtin p b) args
  where
    BuiltinExpr p b args = App (Builtin p b) args

getBuiltinApp :: Expr var builtin -> Maybe (Provenance, builtin, [Arg var builtin])
getBuiltinApp = \case
  Builtin p b -> Just (p, b, [])
  App (Builtin p b) args -> Just (p, b, NonEmpty.toList args)
  _ -> Nothing

getFreeVarApp :: Expr var builtin -> Maybe (Provenance, Identifier, [Arg var builtin])
getFreeVarApp = \case
  FreeVar p b -> Just (p, b, [])
  App (FreeVar p b) args -> Just (p, b, NonEmpty.toList args)
  _ -> Nothing
