module Vehicle.Compile.Type.Constraint.PolarityAnnotationRestrictions
  ( assertUnquantifiedPolarity,
    checkNetworkType,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Data.Builtin.Polarity
import Vehicle.Data.Code.Value

checkNetworkType ::
  forall m.
  (MonadTypeChecker PolarityBuiltin m) =>
  DeclProvenance ->
  GluedType PolarityBuiltin ->
  m (Type PolarityBuiltin)
checkNetworkType (_, p) networkType = case normalised networkType of
  -- \|Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal. Returns a function that
  -- prepends the max linearity constraint.
  VPi binder result -> do
    let inputPol = quote mempty 0 (typeOf binder)
    let outputPol = quote mempty 0 result

    createFreshUnificationConstraint p mempty CheckingAuxiliary (PolarityExpr p Unquantified) inputPol
    createFreshUnificationConstraint p mempty CheckingAuxiliary (PolarityExpr p Unquantified) outputPol
    return $ unnormalised networkType
  _ -> compilerDeveloperError "Invalid network type should have been caught by the main type system"

assertUnquantifiedPolarity ::
  (MonadTypeChecker PolarityBuiltin m) =>
  DeclProvenance ->
  GluedType PolarityBuiltin ->
  m (Type PolarityBuiltin)
assertUnquantifiedPolarity (_, p) t = do
  createFreshUnificationConstraint p mempty CheckingAuxiliary (PolarityExpr p Unquantified) (unnormalised t)
  return $ unnormalised t
