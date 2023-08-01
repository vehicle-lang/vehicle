module Vehicle.Backend.LossFunction.TypeSystem.AnnotationRestrictions
  ( checkParameterType,
    checkNetworkType,
    checkDatasetType,
  )
where

import Control.Monad (when)
import Vehicle.Backend.LossFunction.TypeSystem.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Core
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Interface
import Vehicle.Expr.DeBruijn
import Vehicle.Expr.Normalised

checkNetworkType ::
  forall m.
  (MonadTypeChecker LossBuiltin m) =>
  DeclProvenance ->
  GluedType LossBuiltin ->
  m (Type Ix LossBuiltin)
checkNetworkType _ networkType = return $ unnormalised networkType

checkDatasetType ::
  forall m.
  (MonadTypeChecker LossBuiltin m) =>
  DeclProvenance ->
  GluedType LossBuiltin ->
  m (Type Ix LossBuiltin)
checkDatasetType _ datasetType = return $ unnormalised datasetType

checkParameterType ::
  (MonadTypeChecker LossBuiltin m) =>
  ParameterSort ->
  DeclProvenance ->
  GluedType LossBuiltin ->
  m (Type Ix LossBuiltin)
checkParameterType _sort (_, p) parameterType = do
  let unnormType = unnormalised parameterType
  -- Boolean parameters should remain booleans, rather than be converted to a loss.
  when (isNMeta $ normalised parameterType) $ do
    createFreshUnificationConstraint p mempty CheckingAuxiliary (BoolType p) unnormType
  return unnormType
