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
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Vehicle.Backend.Solver.QueryCompilation.Core (MonadQueryCompilation)
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (lookupNetworkInfo)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (NetworkName)
import Vehicle.Data.Bound
import Vehicle.Data.Bound.FourierMotzkinElimination (fourierMotzkinTensorBoundsElimination)
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr (ConstantLike)
import Vehicle.Data.Code.Value (boundVariablesIn)
import Vehicle.Data.MaybeTrivial (MonadMaybeTrivial (..))
import Vehicle.Data.Tensor (HasShape (..), RatTensor, TensorShape)
import Vehicle.Data.Tensor.Traversal (MonadTraverseTensor, PartiallyKnownTensorShape, toPartialShape)
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
  m (BoundedAssertions SliceVariable RatTensor)
findInputVariableBounds metaNetworkApps constraints = do
  logCompilerSection2 MaxDetail "network variable bounds checks" $
    runMonadBoundsT metaNetworkApps $ do
      -- Search through the list of constraints for bounds
      boundedAssertions <- findBoundsInConjuncts constraints

      logDebugM MaxDetail $ do
        let Partial bounds assertions = boundedAssertions
        assertionsDoc <- prettyFriendlyInCtx assertions
        boundsDoc <- prettyBounds bounds
        return $
          "Found bounds:"
            <> lineIndent boundsDoc
            <> line
            <> "remaining assertions:"
            <> lineIndent assertionsDoc

      -- Check that all bounds present
      checkAllBoundsPresent boundedAssertions

--------------------------------------------------------------------------------
-- MonadSearch

type BoundsState =
  ( PropertyMetaData,
    GlobalCtx,
    Map NetworkInputTensorVariable (NetworkName, NetworkApplicationInfo, TensorShape)
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
            parentShape = toPartialShape (shapeOf nestedSliceVar) Nothing,
            indices = indices
          }

--------------------------------------------------------------------------------
-- Bound checking

checkAllBoundsPresent ::
  (MonadBounds m, MonadMaybeTrivial m) =>
  PartiallyBoundedAssertions ->
  m (BoundedAssertions SliceVariable RatTensor)
checkAllBoundsPresent (Partial allPartialbounds assertions) = do
  (PropertyMetaData {..}, _, inputVariableMapping) <- ask

  errorsAndFinalBounds <- forM (Map.toList inputVariableMapping) $ \(var, (networkName, appInfo, varShape)) -> do
    let errorCase indices = return $ Left (networkName, inputValue appInfo, findUnboundedVariables appInfo, indices)
    case Map.lookup var allPartialbounds of
      Nothing -> errorCase wholeTensorUnbounded
      Just partialBounds -> do
        let partialShape = toPartialShape varShape Nothing
        missingIndicesOrFlattenedBounds <- fourierMotzkinTensorBoundsElimination partialShape partialBounds
        case missingIndicesOrFlattenedBounds of
          Left missingIndices -> errorCase missingIndices
          Right _bounds -> Right <$> lowerTensorBounds partialBounds varShape

  let (missingBounds, boundedVariables) = partitionEithers errorsAndFinalBounds
  case missingBounds of
    [] -> return ()
    i : is -> do
      nameCtx <- getCompleteNamedCtx
      throwError $ UnboundedNetworkInputVariables propertyProvenance nameCtx (i :| is)

  -- If Marabou, then warn if all inputs are constant.
  -- See https://github.com/NeuralNetworkVerification/Marabou/issues/670
  let domains = fmap (fmap valueBounds) boundedVariables
  let formatID = queryFormatID queryFormat
  when (queryFormatID queryFormat == MarabouQueries && all (all isEquality) domains) $
    logWarning $
      AllConstantNetworkInputVars formatID propertyAddress

  let boundsUnsatisfiable = not (all (all isSatisfiable) domains)
  if boundsUnsatisfiable
    then trivial False
    else case assertions of
      [] -> trivial True
      a : as ->
        return $
          BoundedAssertions
            { variableBounds = concat boundedVariables,
              assertions = ConjunctAll (a :| as)
            }

findUnboundedVariables :: NetworkApplicationInfo -> [Lv]
findUnboundedVariables appInfo =
  -- TODO we actually need to do this recursively on any network variables that
  -- live in this set.
  Set.toList $ boundVariablesIn $ inputValue appInfo

-- | Takes in bounds over a tensor and tries to compute a single lower and upper bound for the
-- whole tensor. If it fails then it returns a list of the indices of the tensor which
-- are unbounded.
--
-- NOTE: this function is currently unsound as at the moment it discards the strictness information.
-- See https://github.com/vehicle-lang/vehicle/issues/74
lowerTensorBounds ::
  (MonadLogger m, MonadReadableNameContext m) =>
  TensorBounds RatTensor ->
  TensorShape ->
  m [BoundedValue NetworkIOElementVariable (Domain Rational)]
lowerTensorBounds TensorBounds {..} =
  lowerNestedSliceBounds (SliceBounds mempty mempty) (Just tensorSliceBounds)

lowerNestedSliceBounds ::
  forall m.
  (MonadLogger m, MonadReadableNameContext m) =>
  SliceBounds RatTensor ->
  Maybe (NestedSliceBounds RatTensor) ->
  TensorShape ->
  m [BoundedValue NetworkIOElementVariable (Domain Rational)]
lowerNestedSliceBounds inheritedBounds maybeNestedSliceBounds shape = do
  let currentLowerBounds = lowerBounds inheritedBounds <> maybe mempty (lowerBounds . sliceBounds) maybeNestedSliceBounds
  let currentUpperBounds = upperBounds inheritedBounds <> maybe mempty (upperBounds . sliceBounds) maybeNestedSliceBounds
  let maybeLowerBound = andBoundList currentLowerBounds
  let maybeUpperBound = andBoundList currentUpperBounds

  case shape of
    [] -> do
      let allBounds = _
      return [BoundedValue _ (Domain _ _)]
    d : ds -> do
      let maybeChildBounds = childSliceBounds =<< maybeNestedSliceBounds
      case maybeChildBounds of
        Nothing -> return $ concat <$> traverse _ _
        Just xs -> _

{-
case maybeChildBounds of
  Nothing -> case andBoundList (getBound sliceBounds) of
    Just bounds -> return $ Right bounds
    Nothing -> do
      indices <- currentIndices
      return $ Left [indices]
  Just childTensorBounds -> do
    childErrorOrBounds <- traverseTensorRows go childTensorBounds
    let (missingChildIndices, childBounds) = partitionEithers childErrorOrBounds
    case andBoundList (getBound sliceBounds) of
      Nothing -> case missingChildIndices of
        i : is -> return $ Left $ concatNonEmpty $ i :| is
        [] -> return $ Right $ stackBounds childBounds
      Just bound -> do
        case missingChildIndices of
          _ : _ -> do
            let boundElements = unstackBounds bound
            let combineBound boundElement = either (const boundElement) (andBound boundElement)
            let combinedElements = zipWith combineBound boundElements childErrorOrBounds
            return $ Right $ stackBounds combinedElements
          [] -> do
            let childBound = stackBounds childBounds
            return $ Right $ andBound bound childBound
-}

{-

reduceDomain ::
  forall m.
  (MonadQueryCompilation m) =>
  BoundedValue NetworkInputTensorVariable (Domain RatTensor) ->
  m [BoundedValue NetworkIOElementVariable (Domain Rational)]
reduceDomain (BoundedValue inputTensorVar bounds) = go (toSliceVar inputTensorVar, bounds)
  where
    go ::
      (SliceVariable, Domain RatTensor) ->
      m [BoundedValue NetworkIOElementVariable (Domain Rational)]
    go (var, Domain lowerBound upperBound) =
      case shapeOf lowerBound of
        [] -> do
          let elementVar = coerce var
          let lowerValue = fmap extractRationalConstant lowerBound
          let upperValue = fmap extractRationalConstant upperBound
          let domain = Domain lowerValue upperValue
          return [BoundedValue elementVar domain]
        _ : _ -> do
          childVars <- lookupChildVariablesCertain var
          let lowerBounds = unstackBounds lowerBound
          let upperBounds = unstackBounds upperBound
          let domains = zipWith Domain lowerBounds upperBounds
          concat <$> zipWithM (curry go) childVars domains

-}
