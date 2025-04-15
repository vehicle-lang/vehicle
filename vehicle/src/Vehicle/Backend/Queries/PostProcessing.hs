module Vehicle.Backend.Queries.PostProcessing
  ( compilePartitionsToQueries,
  )
where

import Control.Monad (forM, unless, when)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (get)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldlM)
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.List (sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (isZeroDimensional, tensorShape, tensorToList)
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface
import Vehicle.Verify.Specification (QueryMetaData (..), UserVariableCompilationStep (..), VariableCompilationTrace (..), VariableStore, getQueryVariables)
import Vehicle.Verify.Specification.IO (writeVerificationQuery)

--------------------------------------------------------------------------------
-- Main entry point

-- | Converts a set of partitions to a set of individual queries
compilePartitionsToQueries ::
  (MonadLogger m, MonadStdIO m, MonadSupply QueryID m) =>
  GlobalCtx ->
  PropertyMetaData ->
  Partitions UserOrNetworkTensorVariable ->
  m (DisjunctAll QueryMetaData)
compilePartitionsToQueries globalCtx metaData partitions = do
  allQueries <- forM (partitionsToDisjuncts partitions) $ \partition -> do
    logCompilerPass MaxDetail "compiling partition" $ do
      networkVariablesPartition <- checkNoUserVarsRemaining partition
      (finalVariableCompilationTrace, networkElementVariableAssertions) <-
        reduceAllRemainingNetworkTensorVariables globalCtx networkVariablesPartition
      linearisedAssertions <- traverse lineariseAssertions networkElementVariableAssertions

      let dnfTree = exprToDNF linearisedAssertions
      forM dnfTree $
        compilePartitionToQuery globalCtx metaData finalVariableCompilationTrace
  return $ disjunctDisjuncts allQueries

compilePartitionToQuery ::
  (MonadLogger m, MonadStdIO m, MonadSupply QueryID m) =>
  GlobalCtx ->
  PropertyMetaData ->
  [UserVariableCompilationStep] ->
  ConjunctAll (QueryAssertion NetworkElementVariable) ->
  m QueryMetaData
compilePartitionToQuery globalCtx PropertyMetaData {..} compilationSteps assertions = do
  -- Calculate query address
  queryID <- demand
  let queryAddress = (propertyAddress, queryID)

  logCompilerPass MaxDetail ("compiling query" <+> pretty queryID) $ do
    -- Calculate the meta network for the network
    let metaNetworkApps = calculateMetaNetworkApplications globalCtx assertions
    let metaNetwork = makeMetaNetwork metaNetworkApps

    -- Check if all variables have lower and upper bounds
    checkIfNetworkInputsBounded globalCtx (queryFormatID queryFormat) queryAddress metaNetworkApps assertions

    -- Convert to query variables
    (variableStore, queryAssertions) <-
      compileQueryVariables globalCtx (compileVariable queryFormat) metaNetworkApps assertions

    logDebug MaxDetail $ "Variable mapping:" <+> pretty variableStore

    -- Construct the meta-data object
    let reconstruction = Reconstruction compilationSteps
    let queryMetaData = QueryMetaData queryAddress metaNetwork variableStore reconstruction
    let queryContents = QueryContents (getQueryVariables variableStore) queryAssertions

    -- Actually compile to the query to the format
    queryText <- compileQuery queryFormat queryAddress queryContents

    -- Write out the query to disk
    case outputLocation of
      Nothing -> programOutput $ line <> line <> pretty queryAddress <> line <> pretty queryText
      Just folder -> writeVerificationQuery queryFormat folder (queryMetaData, queryText)

    return queryMetaData

--------------------------------------------------------------------------------
-- Step 1: Reduce tensor equalities to a series of rational equalities and
-- checks that the expression only contains network variables.

reduceAllRemainingNetworkTensorVariables ::
  forall m.
  (MonadCompile m) =>
  GlobalCtx ->
  Partition NetworkTensorVariable ->
  m (Partition NetworkElementVariable)
reduceAllRemainingNetworkTensorVariables globalCtx (steps, assertions) = do
  logCompilerPass MaxDetail "eliminating remaining tensor assertions" $ do
    -- Update the compilation trace
    let allTensorVars = Map.toList (networkTensorVariableInfo globalCtx)
    let networkTensorVars = sortOn fst allTensorVars
    let mkStep (var, TensorVariableInfo {..}) = ReconstructNetworkTensor var elementVariables
    let newSteps = foldr (\v -> (mkStep v :)) steps networkTensorVars

    -- Create the assertions
    newAssertions <- go assertions

    return (newSteps, newAssertions)
  where
    go ::
      BooleanExpr (Assertion NetworkTensorVariable) ->
      m (BooleanExpr (Assertion NetworkElementVariable))
    go = \case
      Query x -> convert x
      Disjunct xs -> Disjunct <$> traverse go xs
      Conjunct xs -> Conjunct <$> traverse go xs

    convert ::
      Assertion NetworkTensorVariable ->
      m (BooleanExpr (Assertion NetworkElementVariable))
    convert (NormalisedRelation relation linearExpr)
      | isZeroDimensional linearExpr =
          return $ Query $ NormalisedRelation relation _
      | otherwise = do
          let rationalEqualities = reduceTensorExpr globalCtx linearExpr
          let reducedAssertions = fmap (Query . NormalisedRelation OEq) rationalEqualities
          go $ Conjunct $ ConjunctAll (NonEmpty.fromList reducedAssertions)

--------------------------------------------------------------------------------
-- Step 2: linearise the query

lineariseAssertions ::
  (MonadCompile m) =>
  Assertion NetworkElementVariable ->
  m (QueryAssertion NetworkElementVariable)
lineariseAssertions (NormalisedRelation relation (Sparse coefficients constant)) = do
  let finalRelation = relationToQueryRelation relation
  let rationalVarCoefs = swap <$> Map.toList coefficients
  finalLHS <- case rationalVarCoefs of
    (c : cs) -> return $ c :| cs
    [] -> compilerDeveloperError "Found trivial assertion"

  let finalRHS = -extractRationalConstant constant
  return $
    QueryAssertion
      { lhs = finalLHS,
        rel = finalRelation,
        rhs = finalRHS
      }

--------------------------------------------------------------------------------
-- Step 3: calculate the actual set of network applications involved

calculateMetaNetworkApplications ::
  (Traversable f) =>
  GlobalCtx ->
  f (QueryAssertion NetworkElementVariable) ->
  [NetworkApplicationReplacement]
calculateMetaNetworkApplications globalCtx@GlobalCtx {..} assertions = do
  -- First calculate the set of network applications actually used in the query
  let referencedVars = foldMap queryAssertionVariables assertions
  let networkApps = snd <$> LinkedHashMap.toList networkApplications
  filter (isApplicationUsed globalCtx referencedVars) networkApps
  where
    queryAssertionVariables :: QueryAssertion NetworkElementVariable -> Set NetworkElementVariable
    queryAssertionVariables = Set.fromList . fmap snd . NonEmpty.toList . lhs

isApplicationUsed ::
  GlobalCtx ->
  Set NetworkElementVariable ->
  NetworkApplicationReplacement ->
  Bool
isApplicationUsed globalCtx referencedVars NetworkApplicationReplacement {..} = do
  let lookupVar = tensorToList . getNetworkElementVariables globalCtx
  let appVars = Set.fromList (lookupVar inputVariable <> lookupVar outputVariable)
  not $ Set.disjoint referencedVars appVars

makeMetaNetwork :: [NetworkApplicationReplacement] -> MetaNetwork
makeMetaNetwork = fmap $ \NetworkApplicationReplacement {..} ->
  MetaNetworkEntry (fst networkApp) networkInfo

--------------------------------------------------------------------------------
-- Step 4: query assertions

-- | Checks for presence of under-constrained input variables.
checkIfNetworkInputsBounded ::
  (MonadCompile m, MonadReader PropertyMetaData m) =>
  GlobalCtx ->
  QueryFormatID ->
  QueryAddress ->
  [NetworkApplicationReplacement] ->
  ConjunctAll (QueryAssertion NetworkElementVariable) ->
  m ()
checkIfNetworkInputsBounded globalCtx queryFormatID queryAddress metaNetworkApps constraints = do
  logCompilerPass MaxDetail "network variable bounds checks" $ do
    let appInputElementVariables app = tensorToList $ getNetworkElementVariables globalCtx (inputVariable app)
    let allInputElementVariables = concatMap appInputElementVariables metaNetworkApps

    finalStatuses <- variableConstraintStatus allInputElementVariables constraints

    -- If Marabou, then warn if all inputs are constant.
    -- See https://github.com/NeuralNetworkVerification/Marabou/issues/670
    when (queryFormatID == MarabouQueries && all (== Constant) finalStatuses) $
      logWarning $
        AllConstantNetworkInputVars queryFormatID queryAddress

    -- Check if all inputs are well-specified.
    let unboundedVariables = Map.toList $ Map.mapMaybe toUnderConstrainedStatus finalStatuses
    unless (null unboundedVariables) $ do
      let lookupVar v = lookupLvInBoundCtx (toLv v) (globalBoundVarCtx globalCtx)
      let unboundedVariableNames = fmap (first lookupVar) unboundedVariables
      logWarning $
        UnboundedNetworkInputVariables queryFormatID queryAddress unboundedVariableNames

-- | How the value of a particular value of a variable is constrained.
data VariableConstraintStatus
  = UnderConstrained UnderConstrainedVariableStatus
  | Bounded
  | Constant
  deriving (Show, Eq)

instance Pretty VariableConstraintStatus where
  pretty = \case
    UnderConstrained s -> pretty s
    Bounded -> "Bounded"
    Constant -> "Constant"

instance Semigroup VariableConstraintStatus where
  UnderConstrained r <> UnderConstrained s = case (r, s) of
    (BoundedBelow, BoundedAbove) -> Bounded
    (BoundedAbove, BoundedBelow) -> Bounded
    _ -> UnderConstrained (r <> s)
  UnderConstrained {} <> r = r
  r <> UnderConstrained {} = r
  Bounded <> r = r
  r <> Bounded = r
  Constant <> Constant = Constant

toUnderConstrainedStatus :: VariableConstraintStatus -> Maybe UnderConstrainedVariableStatus
toUnderConstrainedStatus = \case
  UnderConstrained s -> Just s
  _ -> Nothing

variableConstraintStatus ::
  (MonadCompile m) =>
  [NetworkElementVariable] ->
  ConjunctAll (QueryAssertion NetworkElementVariable) ->
  m (Map NetworkElementVariable VariableConstraintStatus)
variableConstraintStatus variables constraints = do
  let initialStatus = Map.fromList (fmap (,UnderConstrained Unconstrained) variables)
  return $ foldr updateStatuses initialStatus constraints
  where
    updateStatuses ::
      QueryAssertion NetworkElementVariable ->
      Map NetworkElementVariable VariableConstraintStatus ->
      Map NetworkElementVariable VariableConstraintStatus
    updateStatuses assertion statuses = case lhs assertion of
      (c, v) :| [] | v `Map.member` statuses -> do
        let status = case rel assertion of
              EqRel -> Constant
              op
                | (c >= 0) `xor` (op == LeRel || op == LtRel) -> UnderConstrained BoundedBelow
                | otherwise -> UnderConstrained BoundedAbove
        Map.insertWith (<>) v status statuses
      _ -> statuses

--------------------------------------------------------------------------------
-- Step 5: compiling query variables

compileQueryVariables ::
  (MonadCompile m) =>
  GlobalCtx ->
  CompileQueryVariable ->
  [NetworkApplicationReplacement] ->
  ConjunctAll (QueryAssertion NetworkElementVariable) ->
  m (VariableStore, ConjunctAll (QueryAssertion QueryVariable))
compileQueryVariables globalCtx compileVariable metaNetworkApps assertions = do
  -- Compute the set of new input and output variables
  let initialState = IndexingState mempty mempty mempty
  let tensorVars = sortOn fst $ Map.toList (tensorVariableInfo globalCtx)
  let usedNetworkTensorVars = Set.fromList $ concatMap (\x -> [inputVariable x, outputVariable x]) metaNetworkApps
  let compileVars = compileTensorVariable compileVariable (globalBoundVarCtx globalCtx) usedNetworkTensorVars
  indexingState@IndexingState {..} <- foldlM compileVars initialState tensorVars

  -- Make the queries more asthetically pleasing
  let prettifiedAssertions = prettifyQueryContents indexingState assertions

  -- Substitute them through the assertions
  let sortedVariableStore = sortOn (\(v, _, _) -> v) variableStore
  let substitution = Map.fromList (mapMaybe (\(v, _, s) -> fmap (v,) s) sortedVariableStore)
  let newAssertions = fmap (substAssertionVariables substitution) prettifiedAssertions

  return (sortedVariableStore, newAssertions)

data IndexingState = IndexingState
  { networkInputVariables :: [NetworkElementVariable],
    networkOutputVariables :: [NetworkElementVariable],
    variableStore :: VariableStore
  }

compileTensorVariable ::
  (MonadCompile m) =>
  CompileQueryVariable ->
  GenericBoundCtx Name ->
  Set NetworkTensorVariable ->
  IndexingState ->
  (NetworkTensorVariable, TensorVariableInfo) ->
  m IndexingState
compileTensorVariable compileQueryVar boundCtx usedNetworkTensorVariables IndexingState {..} (tensorVar, TensorVariableInfo {..}) = do
  let lookupVar v = lookupLvInBoundCtx v boundCtx
  let tensorEntry = (tensorVar, lookupVar tensorVar, Nothing)
  let elementVariableList = tensorToList elementVariables

  (newInputs, newOutputs, elementEntries) <- case tensorVariableType of
    Just inputOrOutput | tensorVar `Set.member` usedNetworkTensorVariables -> do
      let tensorSize = product (tensorShape elementVariables)
      let startingIndex = length $ if inputOrOutput == Input then networkInputVariables else networkOutputVariables
      let queryVariables = fmap (compileQueryVar inputOrOutput) ([startingIndex .. startingIndex + tensorSize] :: [Int])
      let elementEntries = zipWith (\v q -> (v, lookupVar v, Just q)) elementVariableList queryVariables
      case inputOrOutput of
        Input -> return (elementVariableList, mempty, elementEntries)
        Output -> return (mempty, elementVariableList, elementEntries)
    _ -> do
      let elementEntries = fmap (\v -> (v, lookupVar v, Nothing)) elementVariableList
      return (mempty, mempty, elementEntries)

  return $
    IndexingState
      { networkInputVariables = newInputs <> networkInputVariables,
        networkOutputVariables = newOutputs <> networkOutputVariables,
        variableStore = [tensorEntry] <> elementEntries <> variableStore
      }

substAssertionVariables ::
  Map NetworkElementVariable QueryVariable ->
  QueryAssertion NetworkElementVariable ->
  QueryAssertion QueryVariable
substAssertionVariables subst QueryAssertion {..} = do
  let newLHS = fmap (second substVar) lhs
  QueryAssertion {lhs = newLHS, ..}
  where
    substVar :: NetworkElementVariable -> QueryVariable
    substVar var = case Map.lookup var subst of
      Nothing -> developerError "Malformed network variable subsitution"
      Just newVar -> newVar

--------------------------------------------------------------------------------
-- Step 5: prettyify assertions

prettifyQueryContents ::
  IndexingState ->
  ConjunctAll (QueryAssertion NetworkElementVariable) ->
  ConjunctAll (QueryAssertion NetworkElementVariable)
prettifyQueryContents indexingState (ConjunctAll conjuncts) = do
  let optimisedConjuncts = fmap (optimiseAssertionReadability indexingState) conjuncts
  ConjunctAll $ NonEmpty.sortBy compareAssertion optimisedConjuncts

-- | Applies various optimisations to an assertion to improve readability:
optimiseAssertionReadability ::
  IndexingState ->
  QueryAssertion NetworkElementVariable ->
  QueryAssertion NetworkElementVariable
optimiseAssertionReadability IndexingState {..} (QueryAssertion lhs rel rhs) = do
  let variableList = sort networkInputVariables <> sort networkOutputVariables
  let variableIndexMap = Map.fromList $ zip variableList [(0 :: Int) ..]
  let missingVar v = do
        let (_, n, _) = lookupLvInBoundCtx v variableStore
        developerError $ "Missing network variable" <+> pretty n
  let getIndex v = fromMaybe (missingVar v) $ Map.lookup v variableIndexMap
  -- Put positive coefficients before negative ones, inputs before outputs, and then sort by index
  let sortedLHS = NonEmpty.sortWith (\(c, v) -> (negate (abs c), getIndex v)) lhs

  -- Make the properties a tiny bit nicer by checking if all the vars are
  -- negative and if so negating everything.
  let allCoefficientsNegative = all (\(c, _) -> c < 0) sortedLHS
  let (finalLHS, finalRel, finalRHS) =
        if not allCoefficientsNegative
          then (sortedLHS, rel, rhs)
          else do
            let negCoeffNames = fmap (\(c, v) -> (-c, v)) sortedLHS
            let negOp = flipQueryRel rel
            let negConstant = -rhs
            (negCoeffNames, negOp, negConstant)

  QueryAssertion
    { lhs = finalLHS,
      rel = finalRel,
      rhs = finalRHS
    }

compareAssertion :: QueryAssertion NetworkElementVariable -> QueryAssertion NetworkElementVariable -> Ordering
compareAssertion e1 e2 =
  compareExpression (lhs e1) (lhs e2)
    `thenCmp` compare (rel e1) (rel e2)
    `thenCmp` compare (rhs e1) (rhs e2)

compareExpression ::
  NonEmpty (Coefficient, NetworkElementVariable) ->
  NonEmpty (Coefficient, NetworkElementVariable) ->
  Ordering
compareExpression expr1 expr2 =
  compare (length expr1 == 1) (length expr2 == 1) -- Put variable bounds first
    `thenCmp` compare (fmap snd expr1) (fmap snd expr2)
