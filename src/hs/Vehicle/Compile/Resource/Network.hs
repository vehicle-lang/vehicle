module Vehicle.Compile.Resource.Network
  ( getNetworkType
  ) where

import Control.Monad.Except (MonadError(..))

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource.Core

--------------------------------------------------------------------------------
-- Network typing

getNetworkType :: forall m . MonadCompile m
               => Provenance
               -> Identifier
               -> CheckedExpr
               -> m NetworkType
getNetworkType ann ident networkType = getNetworkFunType networkType
  where

  -- |Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal.
  getNetworkFunType :: CheckedExpr -> m NetworkType
  getNetworkFunType tFun = do
    case tFun of
      Pi _ binder result
        | visibilityOf binder /= Explicit -> do
          throwError $ NetworkTypeHasNonExplicitArguments (ident, ann) tFun binder
        | otherwise  -> do
          inputDetails    <- getTensorType Input  (typeOf binder)
          outputDetails   <- getTensorType Output result
          let networkDetails = NetworkType inputDetails outputDetails
          return networkDetails
      _ ->
        throwError $ NetworkTypeIsNotAFunction (ident, ann) tFun

  getTensorType :: InputOrOutput
                -> CheckedExpr
                -> m NetworkTensorType
  getTensorType io tensorType@(TensorType _ tElem dims) = do
    logDebug MaxDetail $ prettyVerbose dims
    case getDimensions dims of
      Just [d] -> NetworkTensorType d <$> getElementType tElem
      Nothing  -> throwError $ NetworkTypeHasVariableSizeTensor (ident, ann) networkType dims io
      Just _   -> throwError $ NetworkTypeHasMultidimensionalTensor (ident, ann) networkType tensorType io
  getTensorType _ _ = typingError

  getElementType :: CheckedExpr -> m NetworkBaseType
  getElementType RatType{} = return NetworkRatType
  getElementType _tElem    = typingError

  typingError :: m a
  typingError = compilerDeveloperError $
      "Invalid parameter type" <+> squotes (prettySimple networkType) <+>
      "should have been caught during type-checking"