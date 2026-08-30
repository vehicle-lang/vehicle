module Vehicle.Backend.Solver.QueryCompilation.InputBoundsCheck
  ( findInputVariableBounds,
  )
where

import Control.Monad (forM, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Vehicle.Backend.Solver.QueryCompilation.Core (MonadQueryCompilation)
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (lookupNetworkInfo)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (NetworkModality (..), NetworkName)
import Vehicle.Data.Bound
import Vehicle.Data.Bound.FourierMotzkinElimination (fourierMotzkinTensorBoundsEliminationWithErrors)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.MaybeTrivial (MonadMaybeTrivial (..))
import Vehicle.Data.Tensor (HasShape (..), RatTensor, TensorShape)
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor.Class (MonadReadableTensorBoundContext, getCompleteNamedCtx, lookupParentTensorVariable)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Core (inputShape)
import Vehicle.Verify.QueryFormat.Core (QueryFormatID (..))
import Vehicle.Verify.QueryFormat.Interface (QueryFormat (..))

--------------------------------------------------------------------------------
-- Interface

-- | Checks for presence of under-constrained input variables.
findInputVariableBounds ::
  (MonadQueryCompilation m, MonadMaybeTrivial m) =>
  NetworkApplications ->
  ConjunctAll LinearAssertion ->
  m (BoundedAssertions NetworkInputTensorVariable SliceVariable RatTensor)
findInputVariableBounds metaNetworkApps constraints = do
  logCompilerSection2 MaxDetail "network variable bounds checks" $
    runMonadBoundsT metaNetworkApps $ do
      -- Search through the list of constraints for bounds
      boundedAssertions <- findBoundsInConjuncts constraints

      logDebugM MaxDetail $ do
        let Partial bounds assertions = boundedAssertions
        assertionsDoc <- prettyFriendlyInCtx assertions
        boundsDoc <- prettyBounds bounds
        return $ "Found bounds:" <> lineIndent boundsDoc <> line <> "remaining assertions:" <> lineIndent assertionsDoc

      -- Check that all bounds present
      checkAllBoundsPresent boundedAssertions

--------------------------------------------------------------------------------
-- MonadSearch

type BoundsState =
  ( PropertyMetaData,
    GlobalCtx,
    Map NetworkInputTensorVariable (NetworkName, NetworkApplicationInfo, NetworkModality TensorShape)
  )

isNetworkTensorInputVar :: BoundsState -> SliceVariable -> Maybe NetworkInputTensorVariable
isNetworkTensorInputVar (_, _, networkNames) var = do
  let tentativeInputVar = coerce var
  if tentativeInputVar `Map.member` networkNames
    then Just tentativeInputVar
    else Nothing

type MonadBounds m =
  ( MonadCompile m,
    MonadReader BoundsState m,
    MonadReadableTensorBoundContext m
  )

runMonadBoundsT ::
  (MonadLogger m, MonadQueryCompilation m) =>
  NetworkApplications ->
  ReaderT BoundsState m a ->
  m a
runMonadBoundsT networkApps action = do
  (propertyMetaData, globalCtx) <- ask
  -- Make the mapping from input variables to names
  let lookupInputShape name = inputShape $ lookupNetworkInfo name (networkCtx propertyMetaData)
  let mkInputVarEntry (name, app) = (inputVariable app, (name, app, lookupInputShape name))
  let inputVariableMapping = Map.fromList $ mkInputVarEntry <$> toListOfApplications networkApps
  -- Run the monad
  runReaderT action (propertyMetaData, globalCtx, inputVariableMapping)

--------------------------------------------------------------------------------
-- Bounds

data PartiallyBoundedAssertions = Partial
  { variableBounds :: Map NetworkInputTensorVariable (TensorBounds RatTensor),
    remainingAssertions :: [LinearAssertion]
  }

andPartiallyBoundedAssertions ::
  (MonadBounds m) =>
  PartiallyBoundedAssertions ->
  PartiallyBoundedAssertions ->
  m PartiallyBoundedAssertions
andPartiallyBoundedAssertions (Partial bounds1 assertions1) (Partial bounds2 assertions2) = do
  let newVariableBounds = Map.unionWith andBounds bounds1 bounds2
  let newAssertions = assertions1 <> assertions2
  return $ Partial newVariableBounds newAssertions

prettyBounds ::
  forall m a.
  (MonadReadableNameContext m) =>
  Map NetworkInputTensorVariable (TensorBounds RatTensor) ->
  m (Doc a)
prettyBounds bounds = vsep <$> traverse boundToDoc (Map.toList bounds)
  where
    boundToDoc :: (NetworkInputTensorVariable, TensorBounds RatTensor) -> m (Doc a)
    boundToDoc (var, partialBounds) = do
      varDoc <- prettyFriendlyInCtx var
      boundDoc <- prettyFriendlyInCtx (BoundedValue var partialBounds)
      return $ varDoc <> ":" <> lineIndent boundDoc

--------------------------------------------------------------------------------
-- Bound search

findBoundsInConjuncts ::
  (MonadBounds m) =>
  ConjunctAll LinearAssertion ->
  m PartiallyBoundedAssertions
findBoundsInConjuncts conjuncts = do
  ConjunctAll (b :| bs) <- traverse findBoundsInAssertion conjuncts
  foldlM andPartiallyBoundedAssertions b bs

findBoundsInAssertion ::
  (MonadBounds m) =>
  LinearAssertion ->
  m PartiallyBoundedAssertions
findBoundsInAssertion assertion = do
  maybeTensorBounds <- tryToConvertToTensorBounds lookupCorrespondingInputVar assertion
  return $ case maybeTensorBounds of
    Nothing ->
      Partial
        { variableBounds = mempty,
          remainingAssertions = [assertion]
        }
    Just (var, bounds) ->
      Partial
        { variableBounds = Map.singleton (coerce var) bounds,
          remainingAssertions = mempty
        }

lookupCorrespondingInputVar ::
  (MonadBounds m) =>
  SliceVariable ->
  m (Maybe VariableInfo)
lookupCorrespondingInputVar var = do
  state <- ask
  nestedSliceVar <- lookupParentTensorVariable var
  let maybeInputVar = isNetworkTensorInputVar state (toSliceVar nestedSliceVar)
  return $ case maybeInputVar of
    Nothing -> Nothing
    Just inputVar -> do
      let indices = findSliceIndices nestedSliceVar var
      Just $
        VariableInfo
          { parentVariable = toTensorVar inputVar,
            parentShape = shapeOf nestedSliceVar,
            indices = indices
          }

--------------------------------------------------------------------------------
-- Bound checking

checkAllBoundsPresent ::
  (MonadBounds m, MonadMaybeTrivial m) =>
  PartiallyBoundedAssertions ->
  m (BoundedAssertions NetworkInputTensorVariable SliceVariable RatTensor)
checkAllBoundsPresent (Partial allPartialbounds assertions) = do
  (PropertyMetaData {..}, _, inputVariableMapping) <- ask
  lv <- getBinderDepth

  errorsAndFinalBounds <- forM (Map.toList inputVariableMapping) $ \(var, (networkName, appInfo, varShape)) -> do
    let errorCase indices = return $ Left (networkName, inputType appInfo, inputValue appInfo, findUnboundedVariables lv appInfo, indices)
    case Map.lookup var allPartialbounds of
      Nothing -> errorCase wholeTensorUnbounded
      Just partialBounds -> do
        missingIndicesOrFlattenedBounds <- case varShape of
          UniModal {} -> fourierMotzkinTensorBoundsEliminationWithErrors partialBounds
          MultiModal _partialShapes -> error "MultiModal IO is not implmeneted yet"
        case missingIndicesOrFlattenedBounds of
          Right bounds -> return $ Right (BoundedValue var bounds)
          Left missingIndices -> errorCase missingIndices

  let (missingBounds, boundedVariables) = partitionEithers errorsAndFinalBounds
  case missingBounds of
    [] -> return ()
    i : is -> do
      nameCtx <- getCompleteNamedCtx
      throwError $ UnboundedNetworkInputVariables propertyProvenance nameCtx (i :| is)

  -- If Marabou, then warn if all inputs are constant.
  -- See https://github.com/NeuralNetworkVerification/Marabou/issues/670
  let domains = fmap valueBounds boundedVariables
  let formatID = queryFormatID queryFormat
  when (queryFormatID queryFormat == MarabouQueries && all isEquality domains) $
    logWarning $
      AllConstantNetworkInputVars formatID propertyAddress

  let boundsUnsatisfiable = not (all isSatisfiable domains)
  if boundsUnsatisfiable
    then trivial False
    else case assertions of
      [] -> trivial True
      a : as ->
        return $
          BoundedAssertions
            { variableBounds = boundedVariables,
              assertions = ConjunctAll (a :| as)
            }

findUnboundedVariables :: Lv -> NetworkApplicationInfo -> [Lv]
findUnboundedVariables ctxSize appInfo = do
  let inputExpr = (unnormalise ctxSize $ inputValue appInfo :: Expr Builtin)
  -- TODO we actually need to do this recursively on any network variables that
  -- live in this set.
  Set.toList $ boundVariablesIn ctxSize inputExpr
