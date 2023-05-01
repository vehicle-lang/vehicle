module Vehicle.Compile.ExpandResources.Network
  ( checkNetwork,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Map qualified as Map
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Resource
import Vehicle.Compile.Type.Subsystem.Standard
import Vehicle.Expr.Normalised

--------------------------------------------------------------------------------
-- Network typing

checkNetwork ::
  forall m.
  (MonadExpandResources m) =>
  NetworkLocations ->
  DeclProvenance ->
  StandardGluedType ->
  m (FilePath, NetworkType)
checkNetwork networkLocations decl@(ident, _) networkType = do
  case Map.lookup (identifierName ident) networkLocations of
    Nothing -> throwError $ ResourceNotProvided decl Network
    Just location -> do
      typ <- getNetworkType decl networkType
      return (location, typ)

-- | Decomposes the Pi types in a network type signature, checking that the
--  binders are explicit and their types are equal.
getNetworkType ::
  forall m.
  (MonadExpandResources m) =>
  DeclProvenance ->
  StandardGluedType ->
  m NetworkType
getNetworkType decl networkType = case normalised networkType of
  VPi binder result
    | visibilityOf binder /= Explicit -> typingError
    | otherwise -> do
        inputDetails <- getTensorType Input (typeOf binder)
        outputDetails <- getTensorType Output result
        let networkDetails = NetworkType inputDetails outputDetails
        return networkDetails
  _ ->
    throwError $ NetworkTypeIsNotAFunction decl networkType
  where
    getTensorType :: InputOrOutput -> StandardNormType -> m NetworkTensorType
    getTensorType io tensorType = do
      (baseType, dims) <- go True tensorType
      return $ NetworkTensorType baseType dims
      where
        go :: Bool -> StandardNormType -> m (NetworkBaseType, TensorDimensions)
        go topLevel = \case
          VTensorType _ dims -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dims io
          VVectorType tElem dim -> do
            d <- getTensorDimension io dim
            (baseType, ds) <- go False tElem
            return (baseType, d : ds)
          t ->
            if topLevel
              then typingError
              else do
                elemType <- getElementType t
                return (elemType, [])

    getTensorDimension :: InputOrOutput -> StandardNormType -> m Int
    getTensorDimension io dim = case dim of
      VNatLiteral n -> return n
      VFreeVar varIdent _ -> do
        implicitParameters <- getInferableParameterContext
        case Map.lookup varIdent implicitParameters of
          Nothing -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dim io
          Just Left {} -> throwError $ NetworkTypeHasImplicitSizeTensor decl networkType varIdent io
          Just (Right (_, _, d)) -> return d
      dims -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dims io

    getElementType :: StandardNormType -> m NetworkBaseType
    getElementType = \case
      VRatType {} -> return NetworkRatType
      _ -> typingError

    typingError :: m a
    typingError =
      compilerDeveloperError $
        "Invalid network type"
          <+> squotes (prettyVerbose $ normalised networkType)
          <+> "should have been caught during type-checking"
