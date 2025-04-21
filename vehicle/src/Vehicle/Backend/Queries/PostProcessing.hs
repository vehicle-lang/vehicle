module Vehicle.Backend.Queries.PostProcessing
  ( compilePartitionsToQueries,
  )
where

import Control.Monad (forM, unless, when)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (gets)
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
  Partitions TensorVariable ->
  m (DisjunctAll QueryMetaData)
compilePartitionsToQueries globalCtx metaData partitions = do
  allQueries <- forM (partitionsToDisjuncts partitions) $ \partition -> do
    logCompilerPass MaxDetail "compiling partition" $ do
      (variableCompilationTrace, networkElementVariableAssertions) <-
        reduceAllRemainingNetworkTensorVariables globalCtx partition
      linearisedAssertions <- traverse lineariseAssertions networkElementVariableAssertions

      let dnfTree = exprToDNF linearisedAssertions
      forM dnfTree $
        compilePartitionToQuery globalCtx metaData variableCompilationTrace
  return $ disjunctDisjuncts allQueries

compilePartitionToQuery ::
  (MonadLogger m, MonadStdIO m, MonadSupply QueryID m) =>
  GlobalCtx ->
  PropertyMetaData ->
  [UserVariableCompilationStep] ->
  ConjunctAll (QueryAssertion NetworkIOElementVariable) ->
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
  Partition TensorVariable ->
  m (Partition NetworkIOElementVariable)
reduceAllRemainingNetworkTensorVariables globalCtx (compilationTrace, assertions) = do
  logCompilerPass MaxDetail "eliminating remaining tensor assertions" $ do
    -- Update the compilation trace
    let networkTensorVars = sort $ Set.toList (networkTensorVariables globalCtx)
    let mkStep var = ReconstructNetworkTensor var (lookupNetworkElementVariables globalCtx var)
    let newSteps = foldr (\v -> (mkStep v :)) compilationTrace networkTensorVars

    -- Create the assertions
    newAssertions <- go assertions

    return (newSteps, newAssertions)
  where
    go ::
      BooleanExpr (Assertion TensorVariable) ->
      m (BooleanExpr (Assertion NetworkIOElementVariable))
    go = \case
      Query x -> convert x
      Disjunct xs -> Disjunct <$> traverse go xs
      Conjunct xs -> Conjunct <$> traverse go xs

    convert ::
      Assertion TensorVariable ->
      m (BooleanExpr (Assertion NetworkIOElementVariable))
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
  Assertion NetworkIOElementVariable ->
  m (QueryAssertion NetworkIOElementVariable)
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
  f (QueryAssertion NetworkIOElementVariable) ->
  [NetworkApplicationReplacement]
calculateMetaNetworkApplications globalCtx@GlobalCtx {..} assertions = do
  -- First calculate the set of network applications actually used in the query
  let referencedVars = foldMap queryAssertionVariables assertions
  let networkApps = snd <$> LinkedHashMap.toList networkApplications
  filter (isApplicationUsed globalCtx referencedVars) networkApps
  where
    queryAssertionVariables :: QueryAssertion NetworkIOElementVariable -> Set NetworkIOElementVariable
    queryAssertionVariables = Set.fromList . fmap snd . NonEmpty.toList . lhs

isApplicationUsed ::
  GlobalCtx ->
  Set NetworkIOElementVariable ->
  NetworkApplicationReplacement ->
  Bool
isApplicationUsed globalCtx referencedVars NetworkApplicationReplacement {..} = do
  let lookupVar = tensorToList . lookupNetworkElementVariables globalCtx
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
  ConjunctAll (QueryAssertion NetworkIOElementVariable) ->
  m ()
checkIfNetworkInputsBounded globalCtx queryFormatID queryAddress metaNetworkApps constraints = do
  logCompilerPass MaxDetail "network variable bounds checks" $ do
    let appInputElementVariables app = tensorToList $ lookupNetworkElementVariables globalCtx (inputVariable app)
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
      let lookupVar v = variableName $ lookupTensorVariableInfo v globalCtx
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
  [NetworkIOElementVariable] ->
  ConjunctAll (QueryAssertion NetworkIOElementVariable) ->
  m (Map NetworkIOElementVariable VariableConstraintStatus)
variableConstraintStatus variables constraints = do
  let initialStatus = Map.fromList (fmap (,UnderConstrained Unconstrained) variables)
  return $ foldr updateStatuses initialStatus constraints
  where
    updateStatuses ::
      QueryAssertion NetworkIOElementVariable ->
      Map NetworkIOElementVariable VariableConstraintStatus ->
      Map NetworkIOElementVariable VariableConstraintStatus
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
  ConjunctAll (QueryAssertion NetworkIOElementVariable) ->
  m (VariableStore, ConjunctAll (QueryAssertion QueryVariable))
compileQueryVariables globalCtx compileVariable metaNetworkApps assertions = do
  -- Compute the set of new input and output variables
  let initialState = IndexingState mempty mempty mempty
  let compileVarsFn = compileTensorVariable compileVariable globalCtx
  indexingState@IndexingState {..} <- foldlM compileVarsFn initialState metaNetworkApps

  -- Make the queries more asthetically pleasing
  let prettifiedAssertions = prettifyQueryContents indexingState assertions

  -- Substitute them through the assertions
  let sortedVariableStore = sortOn (\(v, _, _) -> v) variableStore
  let substitution = Map.fromList (mapMaybe (\(v, _, s) -> fmap (v,) s) sortedVariableStore)
  let newAssertions = fmap (substAssertionVariables substitution) prettifiedAssertions

  return (sortedVariableStore, newAssertions)

data IndexingState = IndexingState
  { networkInputVariables :: [NetworkIOElementVariable],
    networkOutputVariables :: [NetworkIOElementVariable],
    variableStore :: VariableStore
  }

compileTensorVariable ::
  (MonadCompile m) =>
  CompileQueryVariable ->
  GlobalCtx ->
  IndexingState ->
  NetworkApplicationReplacement ->
  m IndexingState
compileTensorVariable compileQueryVar globalCtx IndexingState {..} NetworkApplicationReplacement {..} = do
  inputChildVars <- gets (lookupChildVariables inputVariable)
  outputChildVars <- gets (lookupChildVariables outputVariable)

  let inputElementVars = maybe [] tensorToList inputChildVars
  let outputElementVars = maybe [] tensorToList outputChildVars

  let inputQueryVars = fmap (compileQueryVar _) inputElementVars
  let outputQueryVars = fmap (compileQueryVar _) outputElementVars

  return $
    IndexingState
      { networkInputVariables = inputElementVars <> networkInputVariables,
        networkOutputVariables = outputElementVars <> networkOutputVariables,
        variableStore = [tensorEntry] <> elementEntries <> variableStore
      }

substAssertionVariables ::
  Map NetworkIOElementVariable QueryVariable ->
  QueryAssertion NetworkIOElementVariable ->
  QueryAssertion QueryVariable
substAssertionVariables subst QueryAssertion {..} = do
  let newLHS = fmap (second substVar) lhs
  QueryAssertion {lhs = newLHS, ..}
  where
    substVar :: NetworkIOElementVariable -> QueryVariable
    substVar var = case Map.lookup var subst of
      Nothing -> developerError "Malformed network variable subsitution"
      Just newVar -> newVar

--------------------------------------------------------------------------------
-- Step 5: prettyify assertions

prettifyQueryContents ::
  IndexingState ->
  ConjunctAll (QueryAssertion NetworkIOElementVariable) ->
  ConjunctAll (QueryAssertion NetworkIOElementVariable)
prettifyQueryContents indexingState (ConjunctAll conjuncts) = do
  let optimisedConjuncts = fmap (optimiseAssertionReadability indexingState) conjuncts
  ConjunctAll $ NonEmpty.sortBy compareAssertion optimisedConjuncts

-- | Applies various optimisations to an assertion to improve readability:
optimiseAssertionReadability ::
  IndexingState ->
  QueryAssertion NetworkIOElementVariable ->
  QueryAssertion NetworkIOElementVariable
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

compareAssertion :: QueryAssertion NetworkIOElementVariable -> QueryAssertion NetworkIOElementVariable -> Ordering
compareAssertion e1 e2 =
  compareExpression (lhs e1) (lhs e2)
    `thenCmp` compare (rel e1) (rel e2)
    `thenCmp` compare (rhs e1) (rhs e2)

compareExpression ::
  NonEmpty (Coefficient, NetworkIOElementVariable) ->
  NonEmpty (Coefficient, NetworkIOElementVariable) ->
  Ordering
compareExpression expr1 expr2 =
  compare (length expr1 == 1) (length expr2 == 1) -- Put variable bounds first
    `thenCmp` compare (fmap snd expr1) (fmap snd expr2)
