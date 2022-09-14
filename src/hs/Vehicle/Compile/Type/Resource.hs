
module Vehicle.Compile.Type.Resource
  ( checkResourceType
  ) where

import Control.Monad.Except (MonadError(..))

import Vehicle.Compile.Prelude
import Vehicle.Compile.Error
import Vehicle.Compile.Type.Meta
import Vehicle.Compile.Normalise as Norm
import Vehicle.Compile.Type.VariableContext
import Vehicle.Compile.Type.Constraint

checkResourceType :: TCM m
                  => ResourceType
                  -> DeclProvenance
                  -> CheckedType
                  -> m CheckedType
checkResourceType resourceType decl@(ident, _) t = do
  let resourceName = pretty resourceType <+> squotes (pretty ident)
  logCompilerPass MidDetail ("checking suitability of the type of" <+> resourceName) $ do
    declCtx <- getNormalisationContext
    let checkFun = case resourceType of
          Parameter         -> checkParameterType
          InferableParameter -> checkInferableParameterType
          Dataset           -> checkDatasetType
          Network           -> checkNetworkType
    normType <- normalise t $ fullNormalisationOptions { Norm.declContext = declCtx }
    alterType <- checkFun decl normType
    return $ alterType t

checkParameterType :: TCM m
                   => DeclProvenance
                   -> CheckedType
                   -> m (CheckedType -> CheckedType)
checkParameterType decl t = do
  case t of
    AnnBoolType{} -> return ()
    IndexType{}   -> return ()
    NatType{}     -> return ()
    IntType{}     -> return ()

    AnnRatType p lin -> do
      let targetLinearity = Builtin p (Linearity Constant)
      addUnificationConstraint LinearityGroup p emptyVariableCtx lin targetLinearity
      return ()

    paramType -> throwError $ ParameterTypeUnsupported decl paramType
  return id

checkInferableParameterType :: TCM m
                           => DeclProvenance
                           -> CheckedType
                           -> m (CheckedType -> CheckedType)
checkInferableParameterType decl t = do
  case t of
    NatType{} -> return ()
    paramType -> throwError $ ParameterTypeUnsupported decl paramType
  return id

checkDatasetType :: forall m . TCM m
                 => DeclProvenance
                 -> CheckedType
                 -> m (CheckedType -> CheckedType)
checkDatasetType decl t = do
  checkContainerType True t
  return id
  where
  checkContainerType :: Bool -> CheckedType -> m ()
  checkContainerType topLevel = \case
    ListType   _ tElem        -> checkContainerType False tElem
    VectorType _ tElem _tDims -> checkContainerType False tElem
    TensorType _ tElem _tDims -> checkContainerType False tElem
    typ                       -> if topLevel
      then throwError $ DatasetTypeUnsupportedContainer decl typ
      else checkDatasetElemType typ

  checkDatasetElemType ::CheckedType -> m ()
  checkDatasetElemType = \case
    BoolType{}   -> return ()
    NatType{}    -> return ()
    IntType{}    -> return ()
    IndexType{}  -> return ()
    AnnRatType p lin -> do
      let targetLinearity = Builtin p (Linearity Constant)
      addUnificationConstraint LinearityGroup p emptyVariableCtx lin targetLinearity
      return ()
    elemType     -> throwError $ DatasetTypeUnsupportedElement decl elemType

checkNetworkType :: forall m . TCM m
                 => DeclProvenance
                 -> CheckedType
                 -> m (CheckedType -> CheckedType)
checkNetworkType decl@(ident, _) networkType = checkFunType networkType
  where

  -- |Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal. Returns a function that
  -- prepends the max linearity constraint.
  checkFunType :: CheckedType -> m (CheckedType -> CheckedType)
  checkFunType = \case
    Pi p binder result
      | visibilityOf binder /= Explicit -> do
        throwError $ NetworkTypeHasNonExplicitArguments decl networkType binder
      | otherwise  -> do
        inputLin  <- checkTensorType Input (typeOf binder)
        outputLin <- checkTensorType Output result

        -- The linearity of the output of a network is the max of 1) Linear (as outputs
        -- are also variables) and 2) the linearity of its input. So prepend this
        -- constraint to the front of the type.
        logDebug MaxDetail "Appending `MaxLinearity` constraint to network type"
        let outputLinProvenance = Linearity $ Linear $ NetworkOutputProvenance p (nameOf ident)
        let linConstraintArgs = [Builtin p outputLinProvenance, inputLin, outputLin]
        let linConstraint = BuiltinTypeClass p MaxLinearity (ExplicitArg p <$> linConstraintArgs)
        return $ \t -> Pi p (IrrelevantInstanceBinder p Nothing linConstraint) t
    _ -> throwError $ NetworkTypeIsNotAFunction decl networkType

  checkTensorType :: InputOrOutput -> CheckedType -> m CheckedType
  checkTensorType io tensorType = go True tensorType
    where
      go :: Bool -> CheckedType -> m CheckedType
      go topLevel = \case
        TensorType _ tElem _ -> go False tElem
        VectorType _ tElem _ -> go False tElem
        elemType             -> if topLevel
          then throwError $ NetworkTypeIsNotOverTensors decl networkType tensorType io
          else checkElementType io elemType

  checkElementType :: InputOrOutput -> CheckedType -> m CheckedType
  checkElementType io = \case
    AnnRatType _ lin -> return lin
    tElem -> throwError $ NetworkTypeHasUnsupportedElementType decl networkType tElem io