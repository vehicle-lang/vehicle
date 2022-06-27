
module Vehicle.Compile.Type.Resource
  ( checkResourceType
  ) where

import Control.Monad.Except (MonadError(..))
import Control.Monad ( when )

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Type.Bidirectional
import Vehicle.Compile.Normalise as Norm
import Vehicle.Compile.Type.VariableContext

checkResourceType :: TCM m
                  => ResourceType
                  -> Provenance
                  -> Identifier
                  -> CheckedExpr
                  -> m ()
checkResourceType resourceType p ident t = do
  let resourceName = pretty resourceType <+> squotes (pretty ident)
  logCompilerPass MidDetail ("checking compatability of type of" <+> resourceName) $ do
    declCtx <- getDeclCtx
    let checkFun = case resourceType of
          Parameter -> checkParameterType
          Dataset   -> checkDatasetType
          Network   -> checkNetworkType
    normType <- normalise t $ defaultNormalisationOptions
      { Norm.declContext = declCtx
      }
    checkFun p ident normType

checkParameterType :: TCM m
                   => Provenance
                   -> Identifier
                   -> CheckedExpr
                   -> m ()
checkParameterType ann ident = \case
  BoolType{}    -> return ()
  IndexType{}   -> return ()
  NatType{}     -> return ()
  IntType{}     -> return ()

  AnnRatType p lin -> do
    let targetLinearity = Builtin p (Linearity Constant)
    addUnificationConstraint p emptyVariableCtx lin targetLinearity
    return ()

  paramType     -> throwError $ ParameterTypeUnsupported ident ann paramType

checkDatasetType :: forall m . TCM m
                 => Provenance
                 -> Identifier
                 -> CheckedExpr
                 -> m ()
checkDatasetType ann ident = checkContainerType True
  where
  checkContainerType :: Bool -> CheckedExpr -> m ()
  checkContainerType topLevel = \case
    ListType   _ tElem        -> checkContainerType False tElem
    TensorType _ tElem _tDims -> checkContainerType False tElem
    typ                       -> if topLevel
      then throwError $ DatasetTypeUnsupportedContainer ident ann typ
      else checkDatasetElemType typ

  checkDatasetElemType ::CheckedExpr -> m ()
  checkDatasetElemType = \case
    BoolType{}   -> return ()
    NatType{}    -> return ()
    IntType{}    -> return ()
    IndexType{}  -> return ()
    AnnRatType p lin -> do
      let targetLinearity = Builtin p (Linearity Constant)
      addUnificationConstraint p emptyVariableCtx lin targetLinearity
      return ()
    elemType     -> throwError $ DatasetTypeUnsupportedElement ident ann elemType

checkNetworkType :: forall m . TCM m
                 => Provenance
                 -> Identifier
                 -> CheckedExpr
                 -> m ()
checkNetworkType _ann ident networkType = checkFunType networkType
  where

  -- |Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal.
  checkFunType :: CheckedExpr -> m ()
  checkFunType = \case
    Pi _ binder result
      | visibilityOf binder /= Explicit -> do
        throwError $ NetworkTypeHasNonExplicitArguments ident networkType binder
      | otherwise  -> do
        checkTensorType Input (typeOf binder)
        checkTensorType Output result
    _ -> throwError $ NetworkTypeIsNotAFunction ident networkType

  checkTensorType :: InputOrOutput -> CheckedExpr -> m ()
  checkTensorType io (TensorType _ tElem _tDims) = checkElementType io tElem
  checkTensorType io tTensor =
    throwError $ NetworkTypeIsNotOverTensors ident networkType tTensor io

  checkElementType :: InputOrOutput -> CheckedExpr -> m ()
  checkElementType io = \case
    (AnnRatType p lin) -> do
      -- Network outputs are also variables that should be counted in the
      -- linearity analysis.
      when (io == Output) $ do
        let linProvenance = NetworkOutputProvenance p (nameOf ident)
        let targetLinearity = Builtin p (Linearity (Linear linProvenance))
        addUnificationConstraint p emptyVariableCtx lin targetLinearity
      return ()
    tElem ->
      throwError $ NetworkTypeHasUnsupportedElementType ident networkType tElem io