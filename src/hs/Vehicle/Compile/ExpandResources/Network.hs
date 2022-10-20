module Vehicle.Compile.ExpandResources.Network
  ( getNetworkType
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState(..))
import Data.Map qualified as Map

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource
import Vehicle.Compile.ExpandResources.Core

--------------------------------------------------------------------------------
-- Network typing

-- |Decomposes the Pi types in a network type signature, checking that the
-- binders are explicit and their types are equal.
getNetworkType :: forall m . MonadExpandResources m
               => DeclProvenance
               -> CheckedType
               -> m NetworkType
getNetworkType decl networkType = case networkType of
  Pi _ binder result
    | visibilityOf binder /= Explicit -> do
      throwError $ NetworkTypeHasNonExplicitArguments decl networkType binder
    | otherwise  -> do
      inputDetails    <- getTensorType Input  (binderType binder)
      outputDetails   <- getTensorType Output result
      let networkDetails = NetworkType inputDetails outputDetails
      return networkDetails
  _ ->
    throwError $ NetworkTypeIsNotAFunction decl networkType

  where
    getTensorType :: InputOrOutput -> CheckedType -> m NetworkTensorType
    getTensorType io tensorType = do
      (baseType, dims) <- go True tensorType
      return $ NetworkTensorType baseType dims
      where
        go :: Bool -> CheckedType -> m (NetworkBaseType, [Int])
        go topLevel = \case
          TensorType _ _ dims    -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dims io
          VectorType _ tElem dim -> do
            d <- getTensorDimension io dim
            (baseType, ds) <- go False tElem
            return (baseType, d : ds)
          t -> if topLevel
            then throwError $ NetworkTypeIsNotOverTensors decl networkType tensorType io
            else do
              elemType <- getElementType t
              return (elemType, [])

    getTensorDimension :: InputOrOutput -> CheckedType -> m Int
    getTensorDimension io dim = case dim of
      NatLiteral _ n -> return n
      FreeVar _ varIdent   -> do
        implicitParameters <- get
        case Map.lookup (nameOf varIdent) implicitParameters of
          Nothing               -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType networkType io
          Just Nothing          -> throwError $ NetworkTypeHasImplicitSizeTensor decl varIdent io
          Just (Just (_, _, d)) -> return d
      dims                 -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dims io

    getElementType :: CheckedType -> m NetworkBaseType
    getElementType = \case
      RatType{}    -> return NetworkRatType
      _            -> typingError

    typingError :: m a
    typingError = compilerDeveloperError $
        "Invalid parameter type" <+> squotes (prettySimple networkType) <+>
        "should have been caught during type-checking"