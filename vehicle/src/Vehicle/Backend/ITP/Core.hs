module Vehicle.Backend.ITP.Core where

import GHC.Stack (HasCallStack)
import Vehicle.Data.AST.Expr.Scoped (Expr)
import Vehicle.Data.Builtin.Interface (BuiltinHasNatType)
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.Interface
import Vehicle.Prelude

-------------------------------------------------------------------------------
-- Utilities

data ComparisonType expr
  = Pointwise [GenericArg expr]
  | Reduced [GenericArg expr]

decideIfPointwiseOrReductionComparison ::
  (HasListExpr Expr Expr builtin, BuiltinHasNatType builtin, HasCallStack) =>
  [GenericArg (Expr builtin)] ->
  ComparisonType (Expr builtin)
decideIfPointwiseOrReductionComparison = \case
  ds : (argExpr -> IDimNil) : as -> Pointwise (ds : as)
  (argExpr -> IDimNil) : ds : as -> Reduced (ds : as)
  _ -> developerError "Unexpected comparison arguments"
