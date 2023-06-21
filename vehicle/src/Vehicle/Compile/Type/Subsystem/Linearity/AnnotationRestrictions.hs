module Vehicle.Compile.Type.Subsystem.Linearity.AnnotationRestrictions
  ( assertConstantLinearity,
    checkNetworkType,
  )
where

import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Linearity.Core
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised

checkNetworkType ::
  forall m.
  (MonadTypeChecker LinearityType m) =>
  DeclProvenance ->
  GluedType LinearityType ->
  m (NormalisableType LinearityType)
checkNetworkType (ident, p) networkType = case normalised networkType of
  -- \|Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal. Returns a function that
  -- prepends the max linearity constraint.
  VPi binder result -> do
    inputLin <- quote mempty 0 (typeOf binder)
    outputLin <- quote mempty 0 result

    -- The linearity of the output of a network is the max of 1) Linear (as outputs
    -- are also variables) and 2) the linearity of its input. So prepend this
    -- constraint to the front of the type.
    logDebug MaxDetail "Appending `MaxLinearity` constraint to network type"
    let outputLinProvenance = Linear $ NetworkOutputProvenance p (nameOf ident)
    let linConstraintArgs = [LinearityExpr p outputLinProvenance, inputLin, outputLin]
    let linConstraint = App p (Builtin p (CType $ LinearityTypeClass MaxLinearity)) (ExplicitArg p <$> linConstraintArgs)
    let linConstraintBinder = Binder p (BinderDisplayForm OnlyType False) (Instance True) Irrelevant linConstraint
    return $ Pi p linConstraintBinder (unnormalised networkType)
  _ -> compilerDeveloperError "Invalid network type should have been caught by the main type system"

assertConstantLinearity ::
  (MonadTypeChecker LinearityType m) =>
  DeclProvenance ->
  GluedType LinearityType ->
  m (NormalisableType LinearityType)
assertConstantLinearity (_, p) t = do
  createFreshUnificationConstraint p mempty CheckingAuxiliary (LinearityExpr p Constant) (unnormalised t)
  return $ unnormalised t
