module Vehicle.Compile.ExpandResources.Network
  ( getNetworkType
  ) where

import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))
import Data.Map qualified as Map

import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Compile.Print
import Vehicle.Expr.Normalised
import Vehicle.Expr.DeBruijn (DBVar(..))

--------------------------------------------------------------------------------
-- Network typing

-- |Decomposes the Pi types in a network type signature, checking that the
-- binders are explicit and their types are equal.
getNetworkType :: forall m . MonadExpandResources m
               => DeclProvenance
               -> GluedType
               -> m NetworkType
getNetworkType decl networkType = case normalised networkType of
  VPi _ binder result
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
    getTensorType :: InputOrOutput -> NormType -> m NetworkTensorType
    getTensorType io tensorType = do
      (baseType, dims) <- go True tensorType
      return $ NetworkTensorType baseType dims
      where
        go :: Bool -> NormType -> m (NetworkBaseType, [Int])
        go topLevel = \case
          VTensorType _ _ dims    -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dims io
          VVectorType _ tElem dim -> do
            d <- getTensorDimension io dim
            (baseType, ds) <- go False tElem
            return (baseType, d : ds)
          t -> if topLevel
            then throwError $ NetworkTypeIsNotOverTensors decl networkType tensorType io
            else do
              elemType <- getElementType t
              return (elemType, [])

    getTensorDimension :: InputOrOutput -> NormType -> m Int
    getTensorDimension io dim = case dim of
      VNatLiteral _ n          -> return n
      VVar _ (Free varIdent) _ -> do
        implicitParameters <- get
        case Map.lookup (nameOf varIdent) implicitParameters of
          Nothing               -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dim io
          Just Nothing          -> throwError $ NetworkTypeHasImplicitSizeTensor decl networkType varIdent io
          Just (Just (_, _, d)) -> return d
      dims                 -> throwError $ NetworkTypeHasVariableSizeTensor decl networkType dims io

    getElementType :: NormType -> m NetworkBaseType
    getElementType = \case
      VRatType{} -> return NetworkRatType
      _          -> typingError

    typingError :: m a
    typingError = compilerDeveloperError $
        "Invalid parameter type" <+> squotes (prettySimple (normalised networkType)) <+>
        "should have been caught during type-checking"
