module Vehicle.Compile.ExpandResources.Network
  ( checkNetwork,
    getTensorRecordShape,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.Force
import Vehicle.Compile.Normalise.TypedValue
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Data.Variable.Bound.Context.Name (MonadNameContext (addNameToContext), runFreshNameBoundContextT)
import Vehicle.Data.Variable.Free.Context (getRecordFieldNames, getRecordFields)
import Vehicle.Verify.Core (NetworkContextInfo (..))

--------------------------------------------------------------------------------
-- Network typing

checkNetwork ::
  forall m.
  (MonadExpandResources m) =>
  DeclProvenance ->
  Type Builtin ->
  FilePath ->
  m NetworkContextInfo
checkNetwork decl networkType filePath = do
  typ <- runFreshNameBoundContextT $ getNetworkType decl networkType
  return $ NetworkContextInfo filePath typ

-- | Decomposes the Pi types in a network type signature, checking that the
--  binders are explicit and their types are equal.
getNetworkType ::
  forall m.
  (MonadExpandResources m, MonadNameContext m) =>
  DeclProvenance ->
  Type Builtin ->
  m NetworkType
getNetworkType decl networkType = do
  forcedType <- forceThunk $ Unforced emptyBoundEnv networkType
  case forcedType of
    VPi binder closure
      | visibilityOf binder /= Explicit ->
          resourceTypingError "network" forcedType
      | otherwise -> do
          inputDetails <- tensorType Input (typeOf binder)
          let resultType = extendClosureWithBound closure binder 0
          outputDetails <- addNameToContext binder $ tensorType Output resultType
          let networkDetails = NetworkType inputDetails outputDetails
          return networkDetails
    _ -> compilerDeveloperError "Should have caught the fact that the network type is not a function during type-checking"
  where
    gluedType :: GluedType Builtin
    gluedType =
      -- This is a hack...
      Glued networkType (Unforced emptyBoundEnv networkType)

    tensorType :: InputOrOutput -> UnforcedType Builtin -> m NetworkIOType
    tensorType io tElem = do
      forcedType <- forceThunk tElem
      case toTypeValue forcedType of
        VTensorType _ dims -> do
          shape <- tensorDimensions io dims
          return $ UniModal (TensorIOType $ NetworkTensorType NetworkRatType shape)
        VTypeFreeVar ident _spine -> do
          fieldNames <- getRecordFieldNames ident
          fields <- getRecordFields ident
          shape <- getTensorRecordShape fields
          return $ UniModal (RecordIOType $ NetworkRecordType NetworkRatType ident shape $ NonEmpty.toList fieldNames)
        _ -> resourceTypingError ("network" <+> pretty io <+> "tensor") forcedType

    tensorDimensions :: InputOrOutput -> UnforcedDims Builtin -> m TensorShape
    tensorDimensions io dims = do
      forcedDims <- forceThunk dims
      case toDimensionsValue forcedDims of
        VDimsNil -> return []
        VDimsCons d ds -> (:) <$> tensorDimension io d <*> tensorDimensions io ds
        _ -> throwError $ NetworkTypeHasVariableSizeTensor decl gluedType dims io

    tensorDimension :: InputOrOutput -> UnforcedDims Builtin -> m Int
    tensorDimension io dim = do
      forcedDim <- forceThunk dim
      case forcedDim of
        INatLiteral n -> return n
        VFreeVar varIdent _ -> do
          implicitParameters <- getInferableParameterContext
          case Map.lookup varIdent implicitParameters of
            Just (_, _, Nothing) -> throwError $ NetworkTypeHasImplicitSizeTensor decl gluedType varIdent io
            Just (_, _, Just (_, _, d)) -> return d
            Nothing -> do
              explicitParameters <- getExplicitParameterContext
              case Map.lookup varIdent explicitParameters of
                Nothing -> throwError $ NetworkTypeHasVariableSizeTensor decl gluedType dim io
                Just value -> tensorDimension io value
        _ -> throwError $ NetworkTypeHasVariableSizeTensor decl gluedType dim io
