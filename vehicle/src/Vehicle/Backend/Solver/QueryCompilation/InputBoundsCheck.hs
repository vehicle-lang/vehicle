module Vehicle.Backend.Solver.QueryCompilation.InputBoundsCheck
  ( findInputVariableBounds,
  )
where

import Control.Monad (forM, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.Foldable (fold)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Vehicle.Backend.Solver.QueryCompilation.Core (MonadQueryCompilation)
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (lookupNetworkInfo)
import Vehicle.Compile.Normalise.Quote (unnormalise)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource (NetworkModality (..), NetworkName)
import Vehicle.Data.Assertion (NormalisedRelation (..), Relation (..))
import Vehicle.Data.Bound
import Vehicle.Data.Bound.FourierMotzkinElimination (fourierMotzkinTensorBoundsEliminationWithErrors)
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr (LinearExpr (..))
import Vehicle.Data.MaybeTrivial (MaybeTrivial (..))
import Vehicle.Data.Tensor (HasShape (..), RatTensor, TensorIndices, TensorShape, at, isTensorOfAll, mapTensor)
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
  (MonadQueryCompilation m) =>
  NetworkApplications ->
  ConjunctAll LinearAssertion ->
  m (MaybeTrivial (BoundedAssertions NetworkInputTensorVariable SliceVariable RatTensor))
findInputVariableBounds metaNetworkApps constraints = do
  logCompilerSection2 MaxDetail "network variable bounds checks" $
    runMonadBoundsT metaNetworkApps $ do
      -- Search through the list of constraints for bounds and equalities
      partiallyBoundedAssertions <- findBoundsInConjuncts constraints

      -- Duplicate bounds across equal input variables and then combine them
      bounds <- combineBounds partiallyBoundedAssertions
      let assertions = remainingAssertions partiallyBoundedAssertions

      logDebugM MaxDetail $ do
        assertionsDoc <- prettyFriendlyInCtx assertions
        boundsDoc <- prettyBounds bounds
        return $ "Found bounds:" <> lineIndent boundsDoc <> line <> "remaining assertions:" <> lineIndent assertionsDoc

      -- Check that all bounds present
      checkAllBoundsPresent bounds assertions

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

-- | The location of a slice of a network input tensor.
data SliceLocation = SliceLocation
  { locationVariable :: NetworkInputTensorVariable,
    locationShape :: TensorShape,
    locationIndices :: TensorIndices
  }
  deriving stock (Eq, Ord)

-- | An equality between two slices of network input tensors.
type SliceEquality = (SliceLocation, SliceLocation)

-- | The bounds found so far. The bounds on each slice are kept separate
-- (rather than being immediately combined into a `TensorBounds` per variable)
-- so that they can be duplicated across any equalities between input
-- variables before they are combined.
data PartiallyBoundedAssertions = Partial
  { inputSliceBounds :: [(SliceLocation, SliceBounds RatTensor)],
    inputEqualities :: [SliceEquality],
    remainingAssertions :: [LinearAssertion]
  }

instance Semigroup PartiallyBoundedAssertions where
  Partial bounds1 equalities1 assertions1 <> Partial bounds2 equalities2 assertions2 =
    Partial (bounds1 <> bounds2) (equalities1 <> equalities2) (assertions1 <> assertions2)

instance Monoid PartiallyBoundedAssertions where
  mempty = Partial mempty mempty mempty

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
findBoundsInConjuncts conjuncts = fold <$> traverse findBoundsInAssertion conjuncts

findBoundsInAssertion ::
  (MonadBounds m) =>
  LinearAssertion ->
  m PartiallyBoundedAssertions
findBoundsInAssertion assertion@(NormalisedRelation rel (Sparse coefficients constant)) = do
  let unchanged = mempty {remainingAssertions = [assertion]}
  case Map.toList coefficients of
    -- Bounds of the form `c*x + b REL 0`
    [(var, coeff)] -> do
      maybeLocation <- lookupInputVarLocation var
      return $ case maybeLocation of
        Nothing -> unchanged
        Just location -> do
          let bounds = convertToSliceBounds rel coeff (mapTensor (\v -> -v / coeff) constant)
          mempty {inputSliceBounds = [(location, bounds)]}
    -- Equalities of the form `c*x - c*y == 0`. Note that the equality is
    -- kept in the list of remaining assertions as it still needs to be
    -- passed to the solver.
    [(var1, coeff1), (var2, coeff2)]
      | rel == OEq && coeff1 == -coeff2 && constant `isTensorOfAll` 0 -> do
          maybeLocation1 <- lookupInputVarLocation var1
          maybeLocation2 <- lookupInputVarLocation var2
          return $ case (maybeLocation1, maybeLocation2) of
            (Just location1, Just location2) -> unchanged {inputEqualities = [(location1, location2)]}
            _ -> unchanged
    _ -> return unchanged

lookupInputVarLocation ::
  (MonadBounds m) =>
  SliceVariable ->
  m (Maybe SliceLocation)
lookupInputVarLocation var = do
  maybeInfo <- lookupCorrespondingInputVar var
  return $ case maybeInfo of
    Nothing -> Nothing
    Just VariableInfo {..} -> Just $ SliceLocation (coerce parentVariable) parentShape indices

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
-- Bound duplication

-- | Nearly all solvers require that each input variable is bounded. In
-- scenarios where a single user variable is used as input to two networks, e.g.
--
--   forall v . 0 <= v <= 1 => f v <= g v
--
-- the generated query will by default only have bounds attached to one input:
--
--   0 <= fx0 <= 1 and
--   fx0 == gx0 and
--   fy0 <= gy0
--
-- This function duplicates any bounds across the equalities between distinct
-- inputs, e.g. for the above it will add the extra bounds:
--
--   0 <= gx0 <= 1
--
-- This includes the cases where equalities occur at a higher dimension
-- than the bounds, and where they occur at a lower dimension than the bounds.
-- The resulting bounds are then combined into a single `TensorBounds` per
-- input variable.
combineBounds ::
  (MonadBounds m) =>
  PartiallyBoundedAssertions ->
  m (Map NetworkInputTensorVariable (TensorBounds RatTensor))
combineBounds Partial {..} = do
  allBounds <-
    if null inputEqualities
      then return inputSliceBounds
      else do
        let symmetricEqualities = concatMap (\(l1, l2) -> [(l1, l2), (l2, l1)]) inputEqualities
        let equalitiesByVar = Map.fromListWith (<>) [(locationVariable l1, [(l1, l2)]) | (l1, l2) <- symmetricEqualities]
        let duplicatedBounds = concatMap (propagateBound equalitiesByVar) inputSliceBounds

        logDebugM MaxDetail $ do
          duplicatedBoundsDoc <- prettyBounds (combineSliceBounds duplicatedBounds)
          return $ "Bounds duplicated via input equalities:" <> lineIndent duplicatedBoundsDoc

        return $ inputSliceBounds <> duplicatedBounds

  return $ combineSliceBounds allBounds

combineSliceBounds :: [(SliceLocation, SliceBounds RatTensor)] -> Map NetworkInputTensorVariable (TensorBounds RatTensor)
combineSliceBounds allSliceBounds = Map.map mkTensorBounds boundsByVariable
  where
    -- Group the slice bounds by the variable they constrain.
    boundsByVariable :: Map NetworkInputTensorVariable (NonEmpty (SliceLocation, SliceBounds RatTensor))
    boundsByVariable = do
      let boundPairs = [(locationVariable location, (location, bounds) :| []) | (location, bounds) <- allSliceBounds]
      NonEmpty.reverse <$> Map.fromListWith (<>) boundPairs

    mkTensorBounds :: NonEmpty (SliceLocation, SliceBounds RatTensor) -> TensorBounds RatTensor
    mkTensorBounds variableBounds@((firstLocation, _) :| _) = do
      let indicesAndBounds = [(locationIndices location, bounds) | (location, bounds) <- NonEmpty.toList variableBounds]
      TensorBounds $ nestedSliceBoundsFromList (locationShape firstLocation) indicesAndBounds

-- | Performs a breadth-first search along the equalities starting from the
-- location of the bound, and returns the corresponding bound for every new
-- slice reached.
--
-- If an equality is at the same or a higher dimension than the current bound
-- then the bound is translated to the corresponding sub-slice of the other side
-- of the equality. If the equality is at a lower dimension than the bound then
-- the bound is restricted to the part of the slice covered by the equality.
propagateBound ::
  Map NetworkInputTensorVariable [SliceEquality] ->
  (SliceLocation, SliceBounds RatTensor) ->
  [(SliceLocation, SliceBounds RatTensor)]
propagateBound equalities (startLocation, startBounds) =
  go (Set.singleton startLocation) [(startLocation, startBounds)]
  where
    go _ [] = []
    go visited ((location, bounds) : queue) = do
      let relevantEqualities = Map.findWithDefault [] (locationVariable location) equalities
      let neighbours = mapMaybe (step location bounds) relevantEqualities
      let newNeighbours = Map.toList $ Map.fromList $ filter (\(l, _) -> l `Set.notMember` visited) neighbours
      let newVisited = foldr (Set.insert . fst) visited newNeighbours
      newNeighbours <> go newVisited (queue <> newNeighbours)

    step ::
      SliceLocation ->
      SliceBounds RatTensor ->
      SliceEquality ->
      Maybe (SliceLocation, SliceBounds RatTensor)
    step location bounds (location1, location2)
      -- The equality is at the same or a higher dimension than the bound.
      | locationIndices location1 `isPrefixOf` indices = do
          let remainingIndices = drop (length (locationIndices location1)) indices
          let newIndices = locationIndices location2 <> remainingIndices
          Just (location2 {locationIndices = newIndices}, bounds)
      -- The equality is at a lower dimension than the bound.
      | indices `isPrefixOf` locationIndices location1 = do
          let remainingIndices = drop (length indices) (locationIndices location1)
          let newBounds = mapSliceBounds (\t -> foldl at t remainingIndices) bounds
          Just (location2, newBounds)
      | otherwise = Nothing
      where
        indices = locationIndices location

--------------------------------------------------------------------------------
-- Bound checking

checkAllBoundsPresent ::
  (MonadBounds m) =>
  Map NetworkInputTensorVariable (TensorBounds RatTensor) ->
  [LinearAssertion] ->
  m (MaybeTrivial (BoundedAssertions NetworkInputTensorVariable SliceVariable RatTensor))
checkAllBoundsPresent allPartialbounds assertions = do
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
    then return $ Trivial False
    else case assertions of
      [] -> return $ Trivial True
      a : as ->
        return $
          NonTrivial $
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
