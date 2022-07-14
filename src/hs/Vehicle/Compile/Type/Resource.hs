
module Vehicle.Compile.Type.Resource
  ( checkResourceType
  ) where

import Control.Monad.Except (MonadError(..))

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
                  -> m CheckedExpr
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
    alterType <- checkFun p ident normType
    return $ alterType t

checkParameterType :: TCM m
                   => Provenance
                   -> Identifier
                   -> CheckedExpr
                   -> m (CheckedExpr -> CheckedExpr)
checkParameterType ann ident t = do
  case t of
    BoolType{}  -> return ()
    IndexType{} -> return ()
    NatType{}   -> return ()
    IntType{}   -> return ()

    AnnRatType p lin -> do
      let targetLinearity = Builtin p (Linearity Constant)
      addUnificationConstraint p emptyVariableCtx lin targetLinearity
      return ()

    paramType -> throwError $ ParameterTypeUnsupported ident ann paramType
  return id

checkDatasetType :: forall m . TCM m
                 => Provenance
                 -> Identifier
                 -> CheckedExpr
                 -> m (CheckedExpr -> CheckedExpr)
checkDatasetType ann ident t = do
  checkContainerType True t
  return id
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
                 -> m (CheckedExpr -> CheckedExpr)
checkNetworkType _ann ident networkType = checkFunType networkType
  where

  -- |Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal. Returns a function that
  -- prepends the max linearity constraint.
  checkFunType :: CheckedExpr -> m (CheckedExpr -> CheckedExpr)
  checkFunType = \case
    Pi p binder result
      | visibilityOf binder /= Explicit -> do
        throwError $ NetworkTypeHasNonExplicitArguments ident networkType binder
      | otherwise  -> do
        inputLin  <- checkTensorType Input (typeOf binder)
        outputLin <- checkTensorType Output result

        -- The linearity of the output of a network is the max of 1) Linear (as outputs
        -- are also variables) and 2) the linearity of its input. So prepend this
        -- constraint to the front of the type.
        let outputLinProvenance = Linearity $ Linear $ NetworkOutputProvenance p (nameOf ident)
        let linConstraintArgs = [inputLin, Builtin p outputLinProvenance, outputLin]
        let linConstraint = BuiltinTypeClass p MaxLinearity (ExplicitArg p <$> linConstraintArgs)
        return $ \t -> Pi p (InstanceBinder p Nothing linConstraint) t
    _ -> throwError $ NetworkTypeIsNotAFunction ident networkType

  checkTensorType :: InputOrOutput -> CheckedExpr -> m CheckedExpr
  checkTensorType io (TensorType _ tElem _tDims) = checkElementType io tElem
  checkTensorType io tTensor =
    throwError $ NetworkTypeIsNotOverTensors ident networkType tTensor io

  checkElementType :: InputOrOutput -> CheckedExpr -> m CheckedExpr
  checkElementType io = \case
    (AnnRatType _ lin) -> return lin
    tElem -> throwError $ NetworkTypeHasUnsupportedElementType ident networkType tElem io