module Vehicle.Compile.Type.Constraint.StandardAnnotationRestrictions
  ( restrictStandardParameterType,
    restrictStandardDatasetType,
    restrictStandardNetworkType,
    restrictStandardPropertyType,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Monad.Class
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value

--------------------------------------------------------------------------------
-- Property

restrictStandardPropertyType ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  DeclProvenance ->
  GluedType Builtin ->
  m ()
restrictStandardPropertyType decl parameterType = go (normalised parameterType)
  where
    go :: VType Builtin -> m ()
    go = \case
      IBoolType {} -> return ()
      IVectorType _ tElem _ -> go tElem
      _ -> throwError $ PropertyTypeUnsupported decl parameterType

--------------------------------------------------------------------------------
-- Parameters

restrictStandardParameterType ::
  (MonadTypeChecker Builtin m) =>
  ParameterSort ->
  DeclProvenance ->
  GluedType Builtin ->
  m (Type Builtin)
restrictStandardParameterType sort = case sort of
  NonInferable -> restrictStandardNonInferableParameterType
  Inferable -> restrictStandardInferableParameterType

restrictStandardNonInferableParameterType ::
  (MonadTypeChecker Builtin m) =>
  DeclProvenance ->
  GluedType Builtin ->
  m (Type Builtin)
restrictStandardNonInferableParameterType decl parameterType = do
  case normalised parameterType of
    IIndexType {} -> return ()
    INatType {} -> return ()
    IRatType {} -> return ()
    IBoolType {} -> return ()
    _ -> throwError $ ParameterTypeUnsupported decl parameterType

  return $ unnormalised parameterType

restrictStandardInferableParameterType ::
  (MonadTypeChecker Builtin m) =>
  DeclProvenance ->
  GluedType Builtin ->
  m (Type Builtin)
restrictStandardInferableParameterType decl parameterType =
  case normalised parameterType of
    INatType {} -> return (unnormalised parameterType)
    _ -> throwError $ InferableParameterTypeUnsupported decl parameterType

--------------------------------------------------------------------------------
-- Datasets

restrictStandardDatasetType ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  DeclProvenance ->
  GluedType Builtin ->
  m (Type Builtin)
restrictStandardDatasetType decl datasetType = do
  checkContainerType True (normalised datasetType)
  return (unnormalised datasetType)
  where
    checkContainerType :: Bool -> VType Builtin -> m ()
    checkContainerType topLevel = \case
      IListType _ tElem -> checkContainerType False tElem
      IVectorType _ tElem _tDims -> checkContainerType False tElem
      remainingType
        | topLevel -> throwError $ DatasetTypeUnsupportedContainer decl datasetType
        | otherwise -> checkDatasetElemType remainingType

    checkDatasetElemType :: VType Builtin -> m ()
    checkDatasetElemType elementType = case elementType of
      INatType {} -> return ()
      IIndexType {} -> return ()
      IRatType {} -> return ()
      IBoolType {} -> return ()
      _ -> throwError $ DatasetTypeUnsupportedElement decl datasetType elementType

--------------------------------------------------------------------------------
-- Networks

restrictStandardNetworkType ::
  forall m.
  (MonadTypeChecker Builtin m) =>
  DeclProvenance ->
  GluedType Builtin ->
  m (Type Builtin)
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
    checkTensorType :: InputOrOutput -> VType Builtin -> m ()
    checkTensorType io = go True
      where
        go :: Bool -> VType Builtin -> m ()
        go topLevel = \case
          IVectorType _ tElem _ -> go False tElem
          elemType ->
            if topLevel
              then throwError $ NetworkTypeIsNotOverTensors decl networkType elemType io
              else checkElementType io elemType

    checkElementType :: InputOrOutput -> VType Builtin -> m ()
    checkElementType io = \case
      IRatType {} -> return ()
      tElem -> throwError $ NetworkTypeHasUnsupportedElementType decl networkType tElem io
