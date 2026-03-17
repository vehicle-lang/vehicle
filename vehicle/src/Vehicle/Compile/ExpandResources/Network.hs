module Vehicle.Compile.ExpandResources.Network
  ( checkNetwork,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Data (Proxy (..))
import Data.Map qualified as Map
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Normalise.NBE (normaliseClosureInCtx)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Resource
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.TypedView (DimensionsValue (..), TensorLikeValue (..), TypeValue (..), toDimensionsValue, toTypeValue)
import Vehicle.Data.Code.Value
import Vehicle.Data.Tensor (TensorShape)
import Vehicle.Data.Variable.Free.Context.Class
import Vehicle.Verify.Core (NetworkContextInfo (..))

-- import Vehicle.Data.AST.Decl (GenericDecl (..))
--------------------------------------------------------------------------------
-- Network typing

checkNetwork ::
  forall m.
  (MonadExpandResources m) =>
  DeclProvenance ->
  GluedType Builtin ->
  FilePath ->
  m NetworkContextInfo
checkNetwork decl networkType filePath = do
  typ <- getNetworkType decl networkType
  return $ NetworkContextInfo filePath typ

-- | Decomposes the Pi types in a network type signature, checking that the
--  binders are explicit and their types are equal.
getNetworkType ::
  forall m.
  (MonadExpandResources m) =>
  DeclProvenance ->
  GluedType Builtin ->
  m NetworkType
getNetworkType decl networkType = case normalised networkType of
  VPi binder closure
    | visibilityOf binder /= Explicit -> typingError
    | otherwise -> do
        inputDetails <- tensorType Input (typeOf binder)
        resultType <- normaliseClosureInCtx mempty binder closure
        outputDetails <- tensorType Output resultType
        let networkDetails = NetworkType (NetworkTensorTypeConstructor inputDetails) (NetworkTensorTypeConstructor outputDetails)
        return networkDetails
  _ -> compilerDeveloperError "Should have caught the fact that the network type is not a function during type-checking"
  where
    tensorType :: InputOrOutput -> VType Builtin -> m NetworkTensorType
    tensorType io t = case toTypeValue t of
      VTensorLike (VRatTensorType dims) -> do
        shape <- tensorDimensions io dims
        return $ NetworkTensorType NetworkRatType shape
      VFreeTypeVar v _spine -> do
        recordType <- getDeclEntry (Proxy @Builtin) v
        case recordType of
          DefRecord p ident _sort _telescope _fields -> compilerDeveloperError $ "matching on DefRecord" <+> pretty ident <+> pretty p
          _ -> compilerDeveloperError "not matching on DefRecord"
      _ -> typingError

    -- is this where we would need to look up the dimensions of the records in the context?
    -- our record type is is an explicit pi binder
    -- VPi binder value -> VPiType binder value
    -- data Closure builtin = Closure (BoundEnv builtin) (Expr builtin)
    -- type VBinder builtin = GenericBinder (Value builtin)

    -- then the type of that binder is VFreeTypeVar
    -- VFreeTypeVar Identifier (Spine Builtin)

    -- then from there we should have a DefRecord
    -- DefRecord
    --   Provenance -- Location in source file.
    --   Identifier -- Name of definition.
    --   (Maybe DefRecordSort) -- List of annotations.
    --   (GenericTelescope expr) -- Type parameters.
    --   (GenericRecordFields expr) -- Fields.

    tensorDimensions :: InputOrOutput -> VType Builtin -> m TensorShape
    tensorDimensions io dims = case toDimensionsValue dims of
      VDimsNil -> return []
      VDimsCons d ds -> (:) <$> tensorDimension io d <*> tensorDimensions io ds
      _ -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dims io

    tensorDimension :: InputOrOutput -> VType Builtin -> m Int
    tensorDimension io dim = case dim of
      INatLiteral n -> return n
      VFreeVar varIdent _ -> do
        implicitParameters <- getInferableParameterContext
        case Map.lookup varIdent implicitParameters of
          Just (_, _, Nothing) -> throwError $ NetworkTypeHasImplicitSizeTensor decl networkType varIdent io
          Just (_, _, Just (_, _, d)) -> return d
          Nothing -> do
            explicitParameters <- getExplicitParameterContext
            case Map.lookup varIdent explicitParameters of
              Nothing -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dim io
              Just value -> tensorDimension io value
      _ -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dim io

    typingError :: m a
    typingError =
      compilerDeveloperError $
        "Invalid network type"
          <+> squotes (prettyVerbose $ normalised networkType)
          <+> "should have been caught during type-checking"

-- gets the equivalent tensor dims for a record
-- type GenericRecordFields expr = [GenericRecordField expr]
getRecordDimsFromFreeCtx ::
  forall m.
  (MonadExpandResources m) =>
  FreeCtxEntry Builtin ->
  m TensorShape
getRecordDimsFromFreeCtx entry = case entry of
  DefRecord _p _ident _sort _telescope fields -> return [length fields]
  _ -> compilerDeveloperError "Unexpectedly not a record."

-- gets the fields for a record in
getRecordFieldsFromFreeCtx ::
  forall m.
  (MonadExpandResources m) =>
  FreeCtxEntry Builtin ->
  VRecordFields Builtin
