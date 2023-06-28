module Vehicle.Expr.Normalisable where

import Data.Aeson (ToJSON)
import Data.Hashable (Hashable)
import Data.Serialize
import GHC.Generics
import Vehicle.Expr.DeBruijn
import Vehicle.Prelude
import Vehicle.Syntax.AST

-------------------------------------------------------------------------------
-- Data type

data NormalisableBuiltin types
  = CConstructor BuiltinConstructor
  | CFunction BuiltinFunction
  | CType types
  deriving (Eq, Ord, Show, Generic)

instance (Pretty types) => Pretty (NormalisableBuiltin types) where
  pretty = \case
    CConstructor f -> pretty f
    CFunction c -> pretty c
    CType t -> pretty t

instance (Serialize types) => Serialize (NormalisableBuiltin types)

instance (Hashable types) => Hashable (NormalisableBuiltin types)

instance (ToJSON types) => ToJSON (NormalisableBuiltin types)

-----------------------------------------------------------------------------
-- Expressions

type NormalisableExpr types = Expr Ix (NormalisableBuiltin types)

type NormalisableBinder types = Binder Ix (NormalisableBuiltin types)

type NormalisableArg types = Arg Ix (NormalisableBuiltin types)

type NormalisableType types = NormalisableExpr types

type NormalisableDecl types = Decl Ix (NormalisableBuiltin types)

type NormalisableProg types = Prog Ix (NormalisableBuiltin types)

type NormalisableTelescope types = Telescope Ix (NormalisableBuiltin types)
