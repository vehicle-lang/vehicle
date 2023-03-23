module Vehicle.Expr.Normalisable where

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
  deriving (Eq, Show, Generic)

instance Pretty types => Pretty (NormalisableBuiltin types) where
  pretty = \case
    CConstructor f -> pretty f
    CFunction c -> pretty c
    CType t -> pretty t

instance Serialize types => Serialize (NormalisableBuiltin types)

instance Hashable types => Hashable (NormalisableBuiltin types)

-----------------------------------------------------------------------------
-- Expressions

type NormalisableExpr types = DBExpr (NormalisableBuiltin types)

type NormalisableBinder types = DBBinder (NormalisableBuiltin types)

type NormalisableArg types = DBArg (NormalisableBuiltin types)

type NormalisableType types = NormalisableExpr types

type NormalisableDecl types = DBDecl (NormalisableBuiltin types)

type NormalisableProg types = DBProg (NormalisableBuiltin types)

type NormalisableTelescope types = DBTelescope (NormalisableBuiltin types)
