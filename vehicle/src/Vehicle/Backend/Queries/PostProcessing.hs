module Vehicle.Backend.Queries.PostProcessing
  ( convertPartitionsToQueries,
  )
where

import Control.Monad (forM, unless, when)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (get)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldlM)
import Data.HashMap.Strict qualified as HashMap
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
import Vehicle.Data.Builtin.Core
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (RationalTensor)
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface
import Vehicle.Verify.Specification (QueryMetaData (..))
import Vehicle.Verify.Specification.IO (writeVerificationQuery)

--------------------------------------------------------------------------------
-- Main entry point

convertPartitionsToQueries ::
  (MonadQueryStructure m, MonadStdIO m, MonadSupply QueryID m) =>
  Partitions ->
  m (DisjunctAll QueryMetaData)
convertPartitionsToQueries partitions = do
  PropertyMetaData {..} <- ask
  globalCtx <- get

  allQueries <- forM (partitionsToDisjuncts partitions) $ \(reconstructionSteps, assertionTree) -> do
    allReconstructionSteps <- reconstructNetworkTensorVars globalCtx reconstructionSteps
    networkVarAssertions <- convertToNetworkRatVarAssertions globalCtx assertionTree
    let dnfTree = exprToDNF networkVarAssertions
    forM dnfTree $ \assertions -> do
      -- Calculate query address
      queryID <- demand
      let queryAddress = (propertyAddress, queryID)

      logCompilerSection MaxDetail ("Compiling query" <+> pretty queryID) $ do
        -- Calculate query meta network
        let metaNetworkApps = calculateMetaNetworkApplications globalCtx assertions

        -- Check if all variables have lower and upper bounds
        checkIfNetworkInputsBounded globalCtx (queryFormatID queryFormat) queryAddress metaNetworkApps assertions

        -- Convert to query variables
        (variableStore, queryAssertions) <-
          compileQueryVariables globalCtx (compileVariable queryFormat) metaNetworkApps assertions

        logDebug MaxDetail $ "Variable mapping:" <+> pretty variableStore

        -- Construct the meta-data object
        let reconstruction = Reconstruction variableStore allReconstructionSteps
        let metaNetwork = makeMetaNetwork metaNetworkApps
        let queryMetaData = QueryMetaData queryAddress metaNetwork reconstruction

        -- Compile to query format
        let queryContents = QueryContents (getQueryVariables reconstruction) queryAssertions
        queryText <- compileQuery queryFormat queryAddress queryContents

        -- Write out the query
        case outputLocation of
          Nothing -> programOutput $ line <> line <> pretty queryAddress <> line <> pretty queryText
          Just folder -> writeVerificationQuery queryFormat folder (queryMetaData, queryText)

        return queryMetaData
  return $ disjunctDisjuncts allQueries

--------------------------------------------------------------------------------
-- Step 0: Add reconstruction steps for network tensor variables.

reconstructNetworkTensorVars ::
  (MonadLogger m) =>
  GlobalCtx ->
  [UserVariableReconstructionStep] ->
  m [UserVariableReconstructionStep]
reconstructNetworkTensorVars GlobalCtx {..} solutions = do
  let networkApplicationInfos = snd <$> LinkedHashMap.toList networkApplications
  let networkVariables = Set.fromList $ concatMap (\r -> [inputVariable r, outputVariable r]) networkApplicationInfos
  let allTensorVars = filter (\(var, _) -> var `Set.member` networkVariables) $ HashMap.toList tensorVariableInfo
  let networkTensorVars = sortOn fst allTensorVars
  let mkStep (var, TensorVariableInfo {..}) = ReconstructTensor OtherVariable tensorVariableShape var elementVariables
  return $ foldr (\v -> (mkStep v :)) solutions networkTensorVars

--------------------------------------------------------------------------------
-- Step 1: Reduce tensor equalities to a series of rational equalities and
-- checks that the expression only contains network variables.

convertToNetworkRatVarAssertions ::
  forall m.
  (MonadCompile m) =>
  GlobalCtx ->
  AssertionTree ->
  m (BooleanExpr (QueryAssertion NetworkElementVariable))
convertToNetworkRatVarAssertions globalCtx = go
  where
    go :: BooleanExpr Assertion -> m (BooleanExpr (QueryAssertion NetworkElementVariable))
    go = \case
      Query x -> convert x
      Disjunct xs -> Disjunct <$> traverse go xs
      Conjunct xs -> Conjunct <$> traverse go xs

    convert :: Assertion -> m (BooleanExpr (QueryAssertion NetworkElementVariable))
    convert = \case
      TensorEq (Equality tensorEquality) -> do
        let rationalEqualities = reduceTensorExpr globalCtx tensorEquality
        let assertions = fmap (Query . RationalEq . Equality) rationalEqualities
        go $ Conjunct $ ConjunctAll (NonEmpty.fromList assertions)
      RationalEq (Equality expr) ->
        Query <$> makeQueryAssertion Equal expr
      RationalIneq (Inequality strict expr) -> do
        let rel = if strict == Strict then LessThan else LessThanOrEqual
        Query <$> makeQueryAssertion rel expr

makeQueryAssertion ::
  (MonadCompile m) =>
  Relation ->
  LinearExpr RationalTensor ->
  m (QueryAssertion NetworkElementVariable)
makeQueryAssertion relation (Sparse coefficients constant) = do
  let finalRelation = case relation of
        Equal -> EqualRel
        LessThan -> OrderRel Lt
        LessThanOrEqual -> OrderRel Le

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
  let lookupVar = getReducedVariablesFor globalCtx
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
  let inputVariables = concatMap (\app -> getReducedVariablesFor globalCtx (inputVariable app)) metaNetworkApps

  finalStatuses <- variableConstraintStatus inputVariables constraints

  -- If Marabou, then warn if all inputs are constant.
  -- See https://github.com/NeuralNetworkVerification/Marabou/issues/670
  when (queryFormatID == MarabouQueries && all (== Constant) finalStatuses) $
    logWarning $
      AllConstantNetworkInputVars queryFormatID queryAddress

  -- Check if all inputs are well-specified.
  let unboundedVariables = Map.toList $ Map.mapMaybe toUnderConstrainedStatus finalStatuses
  unless (null unboundedVariables) $ do
    let lookupVar v = lookupLvInBoundCtx v (globalBoundVarCtx globalCtx)
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
              EqualRel -> Constant
              OrderRel op
                | (c >= 0) `xor` (op == Le || op == Lt) -> UnderConstrained BoundedBelow
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
  let tensorVars = HashMap.toList (tensorVariableInfo globalCtx)
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
  Set TensorVariable ->
  IndexingState ->
  (TensorVariable, TensorVariableInfo) ->
  m IndexingState
compileTensorVariable compileQueryVar boundCtx usedNetworkTensorVariables IndexingState {..} (tensorVar, TensorVariableInfo {..}) = do
  let lookupVar v = lookupLvInBoundCtx v boundCtx
  let tensorEntry = (tensorVar, lookupVar tensorVar, Nothing)

  (newInputs, newOutputs, elementEntries) <- case tensorVariableType of
    Just inputOrOutput | tensorVar `Set.member` usedNetworkTensorVariables -> do
      let tensorSize = product tensorVariableShape
      let startingIndex = length $ if inputOrOutput == Input then networkInputVariables else networkOutputVariables
      let queryVariables = fmap (compileQueryVar inputOrOutput) ([startingIndex .. startingIndex + tensorSize] :: [Int])
      let elementEntries = zipWith (\v q -> (v, lookupVar v, Just q)) elementVariables queryVariables
      case inputOrOutput of
        Input -> return (elementVariables, mempty, elementEntries)
        Output -> return (mempty, elementVariables, elementEntries)
    _ -> do
      let elementEntries = fmap (\v -> (v, lookupVar v, Nothing)) elementVariables
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
  let getIndex v = fromMaybe (developerError "Missing variable") $ Map.lookup v variableIndexMap
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
