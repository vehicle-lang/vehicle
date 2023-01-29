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
import Vehicle.Syntax.AST.Builtin (Builtin, Linearity (..), Polarity (..))
import Vehicle.Syntax.AST.Decl (GenericDecl)
import Vehicle.Syntax.AST.Meta (MetaID)
import Vehicle.Syntax.AST.Name (Name)
import Vehicle.Syntax.AST.Prog (GenericProg)
import Vehicle.Syntax.AST.Provenance (HasProvenance (..), Provenance)
import Vehicle.Syntax.Prelude

--------------------------------------------------------------------------------
-- Universes

type UniverseLevel = Int

data Universe
  = TypeUniv UniverseLevel
  | LinearityUniv
  | PolarityUniv
  deriving (Eq, Ord, Show, Generic)

instance NFData Universe

instance Hashable Universe

instance ToJSON Universe

instance Serialize Universe

instance Pretty Universe where
  pretty = \case
    TypeUniv l -> "Type" <+> pretty l
    LinearityUniv -> "LinearityUniverse"
    PolarityUniv -> "PolarityUniverse"

--------------------------------------------------------------------------------
-- Literals

-- | Type of literals.
-- - The rational literals should `Ratio`, not `Double`
-- - There should be a family of `Float` literals, but we haven't got there yet.
data Literal
  = LUnit
  | LBool Bool
  | LIndex Int Int
  | LNat Int
  | LInt Int
  | LRat Rational
  deriving (Eq, Ord, Show, Generic)

instance NFData Literal

instance Hashable Literal

instance ToJSON Literal

instance Serialize Literal

instance Pretty Literal where
  pretty = \case
    LUnit -> "()"
    LBool x -> pretty x
    LIndex _ x -> pretty x
    LNat x -> pretty x
    LInt x -> pretty x
    LRat x -> pretty x

instance Pretty Rational where
  pretty p = pretty (fromRational p :: Double)

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
  = -- | A universe, used to type types.
    Universe
      Provenance
      Universe
  | -- | User annotation
    Ann
      Provenance
      (Expr binder var) -- The term
      (Expr binder var) -- The type of the term
  | -- | Application of one term to another.
    App
      Provenance
      (Expr binder var) -- Function.
      (NonEmpty (Arg binder var)) -- Arguments.
  | -- | Dependent product (subsumes both functions and universal quantification).
    Pi
      Provenance
      (Binder binder var) -- The bound name
      (Expr binder var) -- (Dependent) result type.
  | -- | Terms consisting of constants that are built into the language.
    Builtin
      Provenance
      Builtin -- Builtin name.
  | -- | Variables that are bound by other expressions
    Var
      Provenance
      var -- Variable name.
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
      (Expr binder var) -- Bound expression body.
      (Binder binder var) -- Bound expression name.
      (Expr binder var) -- Expression body.
  | -- | Lambda expressions (i.e. anonymous functions).
    Lam
      Provenance
      (Binder binder var) -- Bound expression name.
      (Expr binder var) -- Expression body.
  | -- | Built-in literal values e.g. numbers/booleans.
    Literal
      Provenance
      Literal -- Value.
  | -- | A sequence of terms for e.g. list literals.
    LVec
      Provenance
      [Expr binder var] -- List of expressions.
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

instance (NFData binder, NFData var) => NFData (Expr binder var)

instance (ToJSON binder, ToJSON var) => ToJSON (Expr binder var)

instance (Serialize binder, Serialize var) => Serialize (Expr binder var)

type Type = Expr

instance HasProvenance (Expr binder var) where
  provenanceOf = \case
    Universe p _ -> p
    Hole p _ -> p
    Meta p _ -> p
    Ann p _ _ -> p
    App p _ _ -> p
    Pi p _ _ -> p
    Builtin p _ -> p
    Var p _ -> p
    Let p _ _ _ -> p
    Lam p _ _ -> p
    Literal p _ -> p
    LVec p _ -> p

-- * Type of annotations attached to the AST after parsing

-- before being analysed by the compiler
type InputBinding = ()

type InputVar = Name

type InputArg = Arg InputBinding InputVar

type InputBinder = Binder InputBinding InputVar

type InputExpr = Expr InputBinding InputVar

type InputDecl = Decl InputBinding InputVar

type InputProg = Prog InputBinding InputVar

--------------------------------------------------------------------------------
-- Other AST datatypes specialised to the Expr type

type Binder binder var = GenericBinder binder (Expr binder var)

type Arg binder var = GenericArg (Expr binder var)

type Decl binder var = GenericDecl (Expr binder var)

type Prog binder var = GenericProg (Expr binder var)

--------------------------------------------------------------------------------
-- Recursion principles

makeBaseFunctor ''Expr

--------------------------------------------------------------------------------
-- Utilities

renormArgs :: Expr binder var -> NonEmpty (Arg binder var) -> (Expr binder var, NonEmpty (Arg binder var))
renormArgs (App p' fun args') args = renormArgs fun (args' <> args)
renormArgs fun args = (fun, args)

-- Preserves invariant that we never have two nested Apps
normApp :: Provenance -> Expr binder var -> NonEmpty (Arg binder var) -> Expr binder var
normApp p fun args = do
  let (fun', args') = renormArgs fun args
  App p fun' args'

normAppList :: Provenance -> Expr binder var -> [Arg binder var] -> Expr binder var
normAppList _ fun [] = fun
normAppList p fun (arg : args) = normApp p fun (arg :| args)

mkHole :: Provenance -> Name -> Expr binder var
mkHole p name = Hole p ("_" <> name)

isTypeSynonym :: Expr binder var -> Bool
isTypeSynonym = \case
  Universe _ TypeUniv {} -> True
  Pi _ _ res -> isTypeSynonym res
  _ -> False
