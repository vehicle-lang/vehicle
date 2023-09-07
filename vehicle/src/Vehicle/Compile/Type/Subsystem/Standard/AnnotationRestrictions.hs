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
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr

--------------------------------------------------------------------------------
-- Property

restrictStandardPropertyType ::
  forall m.
  (MonadTypeChecker StandardTypingBuiltin m) =>
  DeclProvenance ->
  GluedType StandardTypingBuiltin ->
  m ()
restrictStandardPropertyType decl parameterType = go (normalised parameterType)
  where
    go :: WHNFType StandardTypingBuiltin -> m ()
    go = \case
      VBoolType {} -> return ()
      VVectorType tElem _ -> go tElem
      _ -> throwError $ PropertyTypeUnsupported decl parameterType

--------------------------------------------------------------------------------
-- Parameters

restrictStandardParameterType ::
  (MonadTypeChecker StandardTypingBuiltin m) =>
  ParameterSort ->
  DeclProvenance ->
  GluedType StandardTypingBuiltin ->
  m (Type Ix StandardTypingBuiltin)
restrictStandardParameterType sort = case sort of
  NonInferable -> restrictStandardNonInferableParameterType
  Inferable -> restrictStandardInferableParameterType

restrictStandardNonInferableParameterType ::
  (MonadTypeChecker StandardTypingBuiltin m) =>
  DeclProvenance ->
  GluedType StandardTypingBuiltin ->
  m (Type Ix StandardTypingBuiltin)
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
  (MonadTypeChecker StandardTypingBuiltin m) =>
  DeclProvenance ->
  GluedType StandardTypingBuiltin ->
  m (Type Ix StandardTypingBuiltin)
restrictStandardInferableParameterType decl parameterType =
  case normalised parameterType of
    VNatType {} -> return (unnormalised parameterType)
    _ -> throwError $ InferableParameterTypeUnsupported decl parameterType

--------------------------------------------------------------------------------
-- Datasets

restrictStandardDatasetType ::
  forall m.
  (MonadTypeChecker StandardTypingBuiltin m) =>
  DeclProvenance ->
  GluedType StandardTypingBuiltin ->
  m (Type Ix StandardTypingBuiltin)
restrictStandardDatasetType decl datasetType = do
  checkContainerType True (normalised datasetType)
  return (unnormalised datasetType)
  where
    checkContainerType :: Bool -> WHNFType StandardTypingBuiltin -> m ()
    checkContainerType topLevel = \case
      VListType tElem -> checkContainerType False tElem
      VVectorType tElem _tDims -> checkContainerType False tElem
      VTensorType tElem _tDims -> checkContainerType False tElem
      remainingType
        | topLevel -> throwError $ DatasetTypeUnsupportedContainer decl datasetType
        | otherwise -> checkDatasetElemType remainingType

    checkDatasetElemType :: WHNFType StandardTypingBuiltin -> m ()
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
  (MonadTypeChecker StandardTypingBuiltin m) =>
  DeclProvenance ->
  GluedType StandardTypingBuiltin ->
  m (Type Ix StandardTypingBuiltin)
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
    checkTensorType :: InputOrOutput -> WHNFType StandardTypingBuiltin -> m ()
    checkTensorType io = go True
      where
        go :: Bool -> WHNFType StandardTypingBuiltin -> m ()
        go topLevel = \case
          VTensorType tElem _ -> go False tElem
          VVectorType tElem _ -> go False tElem
          elemType ->
            if topLevel
              then throwError $ NetworkTypeIsNotOverTensors decl networkType elemType io
              else checkElementType io elemType

    checkElementType :: InputOrOutput -> WHNFType StandardTypingBuiltin -> m ()
    checkElementType io = \case
      VRatType {} -> return ()
      tElem -> throwError $ NetworkTypeHasUnsupportedElementType decl networkType tElem io
