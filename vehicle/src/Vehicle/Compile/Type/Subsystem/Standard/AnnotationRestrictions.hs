module Vehicle.Compile.Type.Subsystem.Standard.AnnotationRestrictions
  ( restrictStandardParameterType,
    restrictStandardDatasetType,
    restrictStandardNetworkType,
    restrictStandardPropertyType,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad
import Vehicle.Compile.Type.Subsystem.Standard.Core
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Property

restrictStandardPropertyType ::
  forall m.
  (MonadTypeChecker StandardBuiltinType m) =>
  DeclProvenance ->
  StandardGluedType ->
  m ()
restrictStandardPropertyType decl parameterType = go (normalised parameterType)
  where
    go :: VType StandardBuiltinType -> m ()
    go = \case
      VBoolType {} -> return ()
      VVectorType tElem _ -> go tElem
      _ -> throwError $ PropertyTypeUnsupported decl parameterType

--------------------------------------------------------------------------------
-- Parameters

restrictStandardParameterType ::
  (MonadTypeChecker StandardBuiltinType m) =>
  ParameterSort ->
  DeclProvenance ->
  StandardGluedType ->
  m StandardType
restrictStandardParameterType sort = case sort of
  NonInferable -> restrictStandardNonInferableParameterType
  Inferable -> restrictStandardInferableParameterType

restrictStandardNonInferableParameterType ::
  (MonadTypeChecker StandardBuiltinType m) =>
  DeclProvenance ->
  StandardGluedType ->
  m StandardType
restrictStandardNonInferableParameterType decl parameterType = do
  case normalised parameterType of
    VIndexType {} -> return ()
    VNatType {} -> return ()
    VIntType {} -> return ()
    VRatType {} -> return ()
    VBoolType {} -> return ()
    _ -> throwError $ ParameterTypeUnsupported decl parameterType

  return $ unnormalised parameterType

restrictStandardInferableParameterType ::
  (MonadTypeChecker StandardBuiltinType m) =>
  DeclProvenance ->
  StandardGluedType ->
  m StandardType
restrictStandardInferableParameterType decl parameterType =
  case normalised parameterType of
    VNatType {} -> return (unnormalised parameterType)
    _ -> throwError $ InferableParameterTypeUnsupported decl parameterType

--------------------------------------------------------------------------------
-- Datasets

restrictStandardDatasetType ::
  forall m.
  (MonadTypeChecker StandardBuiltinType m) =>
  DeclProvenance ->
  StandardGluedType ->
  m StandardType
restrictStandardDatasetType decl datasetType = do
  checkContainerType True (normalised datasetType)
  return (unnormalised datasetType)
  where
    checkContainerType :: Bool -> StandardNormType -> m ()
    checkContainerType topLevel = \case
      VListType tElem -> checkContainerType False tElem
      VVectorType tElem _tDims -> checkContainerType False tElem
      VTensorType tElem _tDims -> checkContainerType False tElem
      remainingType
        | topLevel -> throwError $ DatasetTypeUnsupportedContainer decl datasetType
        | otherwise -> checkDatasetElemType remainingType

    checkDatasetElemType :: StandardNormType -> m ()
    checkDatasetElemType elementType = case elementType of
      VNatType {} -> return ()
      VIntType {} -> return ()
      VIndexType {} -> return ()
      VRatType -> return ()
      VBoolType -> return ()
      _ -> throwError $ DatasetTypeUnsupportedElement decl datasetType elementType

--------------------------------------------------------------------------------
-- Networks

restrictStandardNetworkType ::
  forall m.
  (MonadTypeChecker StandardBuiltinType m) =>
  DeclProvenance ->
  StandardGluedType ->
  m StandardType
restrictStandardNetworkType decl networkType = case normalised networkType of
  -- \|Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal. Returns a function that
  -- prepends the max linearity constraint.
  VPi binder result
    | visibilityOf binder /= Explicit ->
        throwError $ NetworkTypeHasNonExplicitArguments decl networkType binder
    | otherwise -> do
        checkTensorType Input (typeOf binder)
        checkTensorType Output result
        return $ unnormalised networkType
  _ -> throwError $ NetworkTypeIsNotAFunction decl networkType
  where
    checkTensorType :: InputOrOutput -> StandardNormType -> m ()
    checkTensorType io = go True
      where
        go :: Bool -> StandardNormType -> m ()
        go topLevel = \case
          VTensorType tElem _ -> go False tElem
          VVectorType tElem _ -> go False tElem
          elemType ->
            if topLevel
              then throwError $ NetworkTypeIsNotOverTensors decl networkType elemType io
              else checkElementType io elemType

    checkElementType :: InputOrOutput -> StandardNormType -> m ()
    checkElementType io = \case
      VRatType {} -> return ()
      tElem -> throwError $ NetworkTypeHasUnsupportedElementType decl networkType tElem io
