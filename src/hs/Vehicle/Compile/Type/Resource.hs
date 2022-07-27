
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
                  -> DeclProvenance
                  -> CheckedExpr
                  -> m CheckedExpr
checkResourceType resourceType decl@(ident, _) t = do
  let resourceName = pretty resourceType <+> squotes (pretty ident)
  logCompilerPass MidDetail ("checking compatability of type of" <+> resourceName) $ do
    declCtx <- getNormalisationContext
    let checkFun = case resourceType of
          Parameter         -> checkParameterType
          ImplicitParameter -> checkImplicitParameterType
          Dataset           -> checkDatasetType
          Network           -> checkNetworkType
    normType <- normalise t $ fullNormalisationOptions { Norm.declContext = declCtx }
    alterType <- checkFun decl normType
    return $ alterType t

checkParameterType :: TCM m
                   => DeclProvenance
                   -> CheckedExpr
                   -> m (CheckedExpr -> CheckedExpr)
checkParameterType decl t = do
  case t of
    BoolType{}  -> return ()
    IndexType{} -> return ()
    NatType{}   -> return ()
    IntType{}   -> return ()

    AnnRatType p lin -> do
      let targetLinearity = Builtin p (Linearity Constant)
      addUnificationConstraint p emptyVariableCtx lin targetLinearity
      return ()

    paramType -> throwError $ ParameterTypeUnsupported decl paramType
  return id

checkImplicitParameterType :: TCM m
                           => DeclProvenance
                           -> CheckedExpr
                           -> m (CheckedExpr -> CheckedExpr)
checkImplicitParameterType decl t = do
  case t of
    NatType{} -> return ()
    paramType -> throwError $ ParameterTypeUnsupported decl paramType
  return id

checkDatasetType :: forall m . TCM m
                 => DeclProvenance
                 -> CheckedExpr
                 -> m (CheckedExpr -> CheckedExpr)
checkDatasetType decl t = do
  checkContainerType True t
  return id
  where
  checkContainerType :: Bool -> CheckedExpr -> m ()
  checkContainerType topLevel = \case
    ListType   _ tElem        -> checkContainerType False tElem
    VectorType _ tElem _tDims -> checkContainerType False tElem
    TensorType _ tElem _tDims -> checkContainerType False tElem
    typ                       -> if topLevel
      then throwError $ DatasetTypeUnsupportedContainer decl typ
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
    elemType     -> throwError $ DatasetTypeUnsupportedElement decl elemType

checkNetworkType :: forall m . TCM m
                 => DeclProvenance
                 -> CheckedExpr
                 -> m (CheckedExpr -> CheckedExpr)
checkNetworkType decl@(ident, _) networkType = checkFunType networkType
  where

  -- |Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal. Returns a function that
  -- prepends the max linearity constraint.
  checkFunType :: CheckedExpr -> m (CheckedExpr -> CheckedExpr)
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
        let outputLinProvenance = Linearity $ Linear $ NetworkOutputProvenance p (nameOf ident)
        let linConstraintArgs = [inputLin, Builtin p outputLinProvenance, outputLin]
        let linConstraint = BuiltinTypeClass p MaxLinearity (ExplicitArg p <$> linConstraintArgs)
        return $ \t -> Pi p (IrrelevantInstanceBinder p Nothing linConstraint) t
    _ -> throwError $ NetworkTypeIsNotAFunction decl networkType

  checkTensorType :: InputOrOutput -> CheckedExpr -> m CheckedExpr
  checkTensorType io (TensorType _ tElem _) = checkElementType io tElem
  checkTensorType io (VectorType _ tElem _) = checkElementType io tElem
  checkTensorType io tTensor =
    throwError $ NetworkTypeIsNotOverTensors decl networkType tTensor io

  checkElementType :: InputOrOutput -> CheckedExpr -> m CheckedExpr
  checkElementType io = \case
    (AnnRatType _ lin) -> return lin
    tElem -> throwError $ NetworkTypeHasUnsupportedElementType decl networkType tElem io