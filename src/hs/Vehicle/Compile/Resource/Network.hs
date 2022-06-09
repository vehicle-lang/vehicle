module Vehicle.Compile.Resource.Network
  ( checkNetworkType
  , extractNetworkType
  ) where

import Control.Monad.Except (MonadError(..))

import Vehicle.Language.Print
import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Resource.Core

--------------------------------------------------------------------------------
-- Network typing

getNetworkType :: Provenance
               -> Identifier
               -> CheckedExpr
               -> Either CompileError NetworkTypeWithUnknownDims
getNetworkType _ ident networkType = do
  (input, output) <- decomposeFun networkType
  inputDetails    <- getTensorType Input  input
  outputDetails   <- getTensorType Output output
  let networkDetails = NetworkType inputDetails outputDetails
  return networkDetails
  where

  -- |Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal.
  decomposeFun :: CheckedExpr
               -> Either CompileError (CheckedExpr, CheckedExpr)
  decomposeFun tFun = do
    case tFun of
      Pi _ binder result
        | visibilityOf binder /= Explicit -> do
          throwError $ NetworkTypeHasNonExplicitArguments ident tFun binder
        | otherwise  -> return (typeOf binder, result)
      _ ->
        throwError $ NetworkTypeIsNotAFunction ident tFun

  getTensorType :: InputOrOutput
                -> CheckedExpr
                -> Either CompileError NetworkTensorTypeWithUnknownDims
  getTensorType io (TensorType _ tElem tDims) = do
    typ   <- getElementType io tElem
    return $ NetworkTensorType tDims typ
  getTensorType io tTensor =
    Left $ NetworkTypeIsNotOverTensors ident networkType tTensor io

  getElementType :: InputOrOutput
                 -> CheckedExpr
                 -> Either CompileError NetworkBaseType
  getElementType _ RatType{} = return NetworkRatType
  getElementType io tElem = do
    throwError $ NetworkTypeHasUnsupportedElementType ident networkType tElem io

checkNetworkType :: MonadCompile m
                 => Provenance
                 -> Identifier
                 -> CheckedExpr
                 -> m ()
checkNetworkType ann ident networkType =
  case getNetworkType ann ident networkType of
    Left err -> throwError err
    Right{}  -> return ()

--------------------------------------------------------------------------------
-- Network standardisation

extractNetworkType :: forall m . MonadCompile m
                   => Provenance
                   -> Identifier
                   -> CheckedExpr
                   -> m NetworkType
extractNetworkType ann ident networkType = do
  typeWithUnknownDims <- case getNetworkType ann ident networkType of
    Left{} -> compilerDeveloperError $
      "Invalid parameter type" <+> squotes (prettySimple networkType) <+>
      "should have been caught during type-checking"
    Right res -> return res

  convertDims typeWithUnknownDims
  where
    convertDims :: NetworkTypeWithUnknownDims -> m NetworkType
    convertDims (NetworkType input output) = NetworkType
      <$> convertDimsTensor Input input
      <*> convertDimsTensor Output output

    convertDimsTensor :: InputOrOutput
                      -> NetworkTensorTypeWithUnknownDims
                      -> m NetworkTensorType
    convertDimsTensor io (NetworkTensorType dims tElem) =
      case getDimensions dims of
        Just [d] -> return $ NetworkTensorType d tElem
        Nothing  -> throwError $ NetworkTypeHasVariableSizeTensor ident networkType dims io
        Just _   -> throwError $ NetworkTypeHasMultidimensionalTensor ident networkType dims io
