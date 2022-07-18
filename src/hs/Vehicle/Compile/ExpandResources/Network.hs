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
               -> CheckedExpr
               -> m NetworkType
getNetworkType decl networkType = case networkType of
  Pi _ binder result
    | visibilityOf binder /= Explicit -> do
      throwError $ NetworkTypeHasNonExplicitArguments decl networkType binder
    | otherwise  -> do
      inputDetails    <- getTensorType Input  (typeOf binder)
      outputDetails   <- getTensorType Output result
      let networkDetails = NetworkType inputDetails outputDetails
      return networkDetails
  _ ->
    throwError $ NetworkTypeIsNotAFunction decl networkType

  where
    getTensorType :: InputOrOutput
                  -> CheckedExpr
                  -> m NetworkTensorType
    getTensorType io tensorType@(TensorType _ tElem dims) =
      NetworkTensorType <$> getTensorDimensions io tensorType dims <*> getElementType tElem
    getTensorType _ _ = typingError

    getTensorDimensions :: InputOrOutput
                        -> CheckedExpr
                        -> CheckedExpr
                        -> m Int
    getTensorDimensions io tensorType dims = case dims of
      SeqExpr _ _ _ [d] -> getTensorDimension io d
      SeqExpr{}         -> throwError $ NetworkTypeHasMultidimensionalTensor decl networkType tensorType io
      _                 -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dims io

    getTensorDimension :: InputOrOutput -> CheckedExpr -> m Int
    getTensorDimension io dim = case dim of
      NatLiteralExpr _ _ n -> return n
      FreeVar _ varIdent   -> do
        implicitParameters <- get
        case Map.lookup (nameOf varIdent) implicitParameters of
          Nothing               -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType networkType io
          Just Nothing          -> throwError $ NetworkTypeHasImplicitSizeTensor decl varIdent io
          Just (Just (_, _, d)) -> return d
      dims                 -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dims io

    getElementType :: CheckedExpr -> m NetworkBaseType
    getElementType RatType{} = return NetworkRatType
    getElementType _tElem    = typingError

    typingError :: m a
    typingError = compilerDeveloperError $
        "Invalid parameter type" <+> squotes (prettySimple networkType) <+>
        "should have been caught during type-checking"