module Vehicle.Backend.ITP.Core where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import GHC.Stack (HasCallStack)
import Vehicle.Compile.Print (prettyVerbose)
import Vehicle.Data.AST.Expr.Scoped
import Vehicle.Data.Builtin.Core (BuiltinFunction (..))
import Vehicle.Data.Builtin.Decidability (DecidabilityBuiltin (..))
import Vehicle.Data.Builtin.Standard.Normalise ()
import Vehicle.Data.Code.Interface
import Vehicle.Prelude

-------------------------------------------------------------------------------
-- Utilities

data ComparisonType expr
  = Pointwise [GenericArg expr]
  | Reduced [GenericArg expr]

decideIfPointwiseOrReductionComparison ::
  (HasListExpr Expr Expr DecidabilityBuiltin, HasCallStack) =>
  [GenericArg (Expr DecidabilityBuiltin)] ->
  ComparisonType (Expr DecidabilityBuiltin)
decideIfPointwiseOrReductionComparison = \case
  _ds : (argExpr -> IDimNil) : as -> Pointwise as
  (argExpr -> IDimNil) : _ds : as -> Reduced as
  args -> developerError $ "Unexpected comparison arguments:" <+> prettyVerbose args

-- | This is a hack to avoid the arguments that the `decideIfPointwiseOrReductionComparison`
-- being prematurely removed. Will hopefully be removed once we have a unified
-- preprocessed datatype for all ITP targets.
builtinAppArgs :: DecidabilityBuiltin -> NonEmpty (Arg DecidabilityBuiltin) -> [Arg DecidabilityBuiltin]
builtinAppArgs b args = case b of
  StandardBuiltinFunction (CompareRatTensor _) -> NonEmpty.toList args
  _ -> NonEmpty.filter (not . wasInsertedByCompiler) args
