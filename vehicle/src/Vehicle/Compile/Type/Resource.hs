module Vehicle.Compile.Type.Resource
  ( checkResourceType,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Normalise.Quote (Quote (..))
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Constraint
import Vehicle.Compile.Type.Monad
import Vehicle.Expr.Normalised

checkResourceType ::
  TCM m =>
  Resource ->
  DeclProvenance ->
  GluedType ->
  m CheckedType
checkResourceType resource decl@(ident, _) resourceType = do
  let resourceName = pretty resource <+> squotes (pretty ident)
  logCompilerPass MidDetail ("checking suitability of the type of" <+> resourceName) $ do
    case resource of
      Parameter -> checkParameterType decl resourceType
      InferableParameter -> checkInferableParameterType decl resourceType
      Dataset -> checkDatasetType decl resourceType
      Network -> checkNetworkType decl resourceType

--------------------------------------------------------------------------------
-- Parameters

checkParameterType ::
  TCM m =>
  DeclProvenance ->
  GluedType ->
  m CheckedType
checkParameterType decl@(_, p) parameterType = do
  case normalised parameterType of
    VIndexType {} -> return ()
    VNatType {} -> return ()
    VIntType {} -> return ()
    VAnnRatType lin -> checkRatIsConstant p lin
    VAnnBoolType lin pol -> checkBoolIsConstant p lin pol
    _ -> throwError $ ParameterTypeUnsupported decl parameterType

  return $ unnormalised parameterType

checkInferableParameterType ::
  TCM m =>
  DeclProvenance ->
  GluedType ->
  m CheckedType
checkInferableParameterType decl parameterType = case normalised parameterType of
  VNatType {} -> return (unnormalised parameterType)
  _ -> throwError $ ParameterTypeUnsupported decl parameterType

--------------------------------------------------------------------------------
-- Datasets

checkDatasetType ::
  forall m.
  TCM m =>
  DeclProvenance ->
  GluedType ->
  m CheckedType
checkDatasetType decl@(_, p) datasetType = do
  checkContainerType True (normalised datasetType)
  return (unnormalised datasetType)
  where
    checkContainerType :: Bool -> NormType -> m ()
    checkContainerType topLevel = \case
      VListType tElem -> checkContainerType False tElem
      VVectorType tElem _tDims -> checkContainerType False tElem
      VTensorType tElem _tDims -> checkContainerType False tElem
      remainingType
        | topLevel -> throwError $ DatasetTypeUnsupportedContainer decl datasetType
        | otherwise -> checkDatasetElemType remainingType

    checkDatasetElemType :: NormType -> m ()
    checkDatasetElemType elementType = case elementType of
      VNatType {} -> return ()
      VIntType {} -> return ()
      VIndexType {} -> return ()
      VAnnRatType lin -> checkRatIsConstant p lin
      VAnnBoolType lin pol -> checkBoolIsConstant p lin pol
      _ -> throwError $ DatasetTypeUnsupportedElement decl datasetType elementType

--------------------------------------------------------------------------------
-- Networks

checkNetworkType ::
  forall m.
  TCM m =>
  DeclProvenance ->
  GluedType ->
  m CheckedType
checkNetworkType decl@(ident, p) networkType = case normalised networkType of
  -- \|Decomposes the Pi types in a network type signature, checking that the
  -- binders are explicit and their types are equal. Returns a function that
  -- prepends the max linearity constraint.
  VPi binder result
    | visibilityOf binder /= Explicit ->
        throwError $ NetworkTypeHasNonExplicitArguments decl networkType binder
    | otherwise -> do
        inputLin <- quote mempty 0 =<< checkTensorType Input (typeOf binder)
        outputLin <- quote mempty 0 =<< checkTensorType Output result

        -- The linearity of the output of a network is the max of 1) Linear (as outputs
        -- are also variables) and 2) the linearity of its input. So prepend this
        -- constraint to the front of the type.
        logDebug MaxDetail "Appending `MaxLinearity` constraint to network type"
        let outputLinProvenance = Linear $ NetworkOutputProvenance p (nameOf ident)
        let linConstraintArgs = [LinearityExpr p outputLinProvenance, inputLin, outputLin]
        let linConstraint = BuiltinTypeClass p (LinearityTypeClass MaxLinearity) (ExplicitArg p <$> linConstraintArgs)
        let linConstraintBinder = Binder p (BinderDisplayForm OnlyType False) (Instance True) Irrelevant () linConstraint
        return $ Pi p linConstraintBinder (unnormalised networkType)
  _ -> throwError $ NetworkTypeIsNotAFunction decl networkType
  where
    checkTensorType :: InputOrOutput -> NormType -> m NormType
    checkTensorType io = go True
      where
        go :: Bool -> NormType -> m NormType
        go topLevel = \case
          VTensorType tElem _ -> go False tElem
          VVectorType tElem _ -> go False tElem
          elemType ->
            if topLevel
              then throwError $ NetworkTypeIsNotOverTensors decl networkType elemType io
              else checkElementType io elemType

    checkElementType :: InputOrOutput -> NormType -> m NormType
    checkElementType io = \case
      VAnnRatType lin -> return lin
      tElem -> throwError $ NetworkTypeHasUnsupportedElementType decl networkType tElem io

--------------------------------------------------------------------------------
-- Utilities

checkRatIsConstant :: TCM m => Provenance -> NormType -> m ()
checkRatIsConstant p lin = do
  let targetLinearity = LinearityExpr p Constant
  ulin <- quote mempty 0 lin
  createFreshUnificationConstraint LinearityGroup p mempty CheckingAuxiliary targetLinearity ulin

checkBoolIsConstant :: TCM m => Provenance -> NormType -> NormType -> m ()
checkBoolIsConstant p lin pol = do
  let targetLinearity = LinearityExpr p Constant
  let targetPolarity = PolarityExpr p Unquantified
  ulin <- quote mempty 0 lin
  upol <- quote mempty 0 pol
  createFreshUnificationConstraint LinearityGroup p mempty CheckingAuxiliary targetLinearity ulin
  createFreshUnificationConstraint PolarityGroup p mempty CheckingAuxiliary targetPolarity upol
