{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Syntax.AST.Expr where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), (<+>))
import Vehicle.Syntax.AST.Arg
import Vehicle.Syntax.AST.Binder
import Vehicle.Syntax.AST.Builtin (Builtin)
import Vehicle.Syntax.AST.Decl (GenericDecl)
import Vehicle.Syntax.AST.Meta (MetaID)
import Vehicle.Syntax.AST.Name (Identifier, Name)
import Vehicle.Syntax.AST.Prog (GenericProg)
import Vehicle.Syntax.AST.Provenance (HasProvenance (..), Provenance)
import Vehicle.Syntax.Prelude

--------------------------------------------------------------------------------
-- Universes

newtype UniverseLevel = UniverseLevel Int
  deriving (Eq, Ord, Show, Generic)

instance NFData UniverseLevel

instance Hashable UniverseLevel

instance ToJSON UniverseLevel

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
data Expr binder var builtin
  = -- | A universe, used to type types.
    Universe
      Provenance
      UniverseLevel
  | -- | User annotation
    Ann
      Provenance
      (Expr binder var builtin) -- The term
      (Expr binder var builtin) -- The type of the term
  | -- | Application of one term to another.
    App
      Provenance
      (Expr binder var builtin) -- Function.
      (NonEmpty (Arg binder var builtin)) -- Arguments.
  | -- | Dependent product (subsumes both functions and universal quantification).
    Pi
      Provenance
      (Binder binder var builtin) -- The bound name
      (Expr binder var builtin) -- (Dependent) result type.
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
  | -- | Let expressions. We have these in the core syntax because we want to
    -- cross compile them to various backends.
    --
    -- NOTE: that the order of the bound expression and the binder is reversed
    -- to better mimic the flow of the context, which makes writing monadic
    -- operations concisely much easier.
    Let
      Provenance
      (Expr binder var builtin) -- Bound expression body.
      (Binder binder var builtin) -- Bound expression name.
      (Expr binder var builtin) -- Expression body.
  | -- | Lambda expressions (i.e. anonymous functions).
    Lam
      Provenance
      (Binder binder var builtin) -- Bound expression name.
      (Expr binder var builtin) -- Expression body.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var, NFData builtin) => NFData (Expr binder var builtin)

instance (ToJSON binder, ToJSON var, ToJSON builtin) => ToJSON (Expr binder var builtin)

instance (Serialize binder, Serialize var, Serialize builtin) => Serialize (Expr binder var builtin)

type Type = Expr

instance HasProvenance (Expr binder var builtin) where
  provenanceOf = \case
    Universe p _ -> p
    Hole p _ -> p
    Meta p _ -> p
    Ann p _ _ -> p
    App p _ _ -> p
    Pi p _ _ -> p
    Builtin p _ -> p
    BoundVar p _ -> p
    FreeVar p _ -> p
    Let p _ _ _ -> p
    Lam p _ _ -> p

-- * Type of annotations attached to the AST after parsing

-- before being analysed by the compiler
type InputBinding = ()

type InputVar = Name

type InputArg = Arg InputBinding InputVar Builtin

type InputBinder = Binder InputBinding InputVar Builtin

type InputExpr = Expr InputBinding InputVar Builtin

type InputDecl = Decl InputBinding InputVar Builtin

type InputProg = Prog InputBinding InputVar Builtin

--------------------------------------------------------------------------------
-- Other AST datatypes specialised to the Expr type

type Binder binder var builtin = GenericBinder binder (Expr binder var builtin)

type Arg binder var builtin = GenericArg (Expr binder var builtin)

type Decl binder var builtin = GenericDecl (Expr binder var builtin)

type Prog binder var builtin = GenericProg (Expr binder var builtin)

--------------------------------------------------------------------------------
-- Recursion principles

makeBaseFunctor ''Expr

--------------------------------------------------------------------------------
-- Utilities

renormArgs :: Expr binder var builtin -> NonEmpty (Arg binder var builtin) -> (Expr binder var builtin, NonEmpty (Arg binder var builtin))
renormArgs (App p' fun args') args = renormArgs fun (args' <> args)
renormArgs fun args = (fun, args)

-- Preserves invariant that we never have two nested Apps
normApp :: Provenance -> Expr binder var builtin -> NonEmpty (Arg binder var builtin) -> Expr binder var builtin
normApp p fun args = do
  let (fun', args') = renormArgs fun args
  App p fun' args'

normAppList :: Provenance -> Expr binder var builtin -> [Arg binder var builtin] -> Expr binder var builtin
normAppList _ fun [] = fun
normAppList p fun (arg : args) = normApp p fun (arg :| args)

mkHole :: Provenance -> Name -> Expr binder var builtin
mkHole p name = Hole p ("_" <> name)

isTypeSynonym :: Expr binder var builtin -> Bool
isTypeSynonym = \case
  Universe {} -> True
  Pi _ _ res -> isTypeSynonym res
  _ -> False

pattern TypeUniverse :: Provenance -> Int -> Expr binder var builtin
pattern TypeUniverse p l = Universe p (UniverseLevel l)

pattern BuiltinExpr ::
  Provenance ->
  builtin ->
  NonEmpty (Arg binder var builtin) ->
  Expr binder var builtin
pattern BuiltinExpr p b args <- App p (Builtin _ b) args
  where
    BuiltinExpr p b args = App p (Builtin p b) args
