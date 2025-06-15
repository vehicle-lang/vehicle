module Vehicle.Backend.Queries.PostProcessing
  ( compilePartitionsToQueries,
  )
where

import Control.Monad (forM, unless, when)
import Control.Monad.Except (MonadError (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly, prettyVerbose)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor as Tensor
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface
import Vehicle.Verify.Specification (QueryMetaData (..), UserVariableCompilationStep (..), VariableCompilationTrace (..), VariableStore (..), getQueryVariables)
import Vehicle.Verify.Specification.IO (writeVerificationQuery)

--------------------------------------------------------------------------------
-- Main entry point

-- | Converts a set of partitions to a set of individual queries
compilePartitionsToQueries ::
  (MonadCompile m, MonadStdIO m, MonadSupply QueryID m) =>
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
  (MonadCompile m, MonadStdIO m, MonadSupply QueryID m) =>
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
    let formatID = queryFormatID queryFormat
    let metaNetworkApps = calculateMetaNetworkApplications globalCtx assertions
    when (length metaNetworkApps > 1 && not (supportsMultipleNetworks queryFormat)) $ do
      throwError $ UnsupportedMultipleNetworkApplications formatID propertyProvenance (fmap networkApp metaNetworkApps)

    let metaNetwork = makeMetaNetwork metaNetworkApps

    -- Check if all variables have lower and upper bounds
    checkIfNetworkInputsBounded globalCtx formatID queryAddress metaNetworkApps assertions

    -- Convert to query variables
    (variableStore, queryAssertions) <-
      compileQueryVariables globalCtx (compileVariable queryFormat) metaNetworkApps assertions

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
    let findElementVars var = coerce $ lookupNetworkElementVariables globalCtx var
    let mkStep var = ReconstructTensorVariable (coerce var) (findElementVars var)
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
      | isZeroDimensional linearExpr = do
          -- TODO maybe we should actually check here rather than just casting
          let castExpr = mapVariables coerce linearExpr
          return $ Query $ NormalisedRelation relation castExpr
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
  let lookupVar = Tensor.toList . lookupNetworkElementVariables globalCtx
  let appVars = Set.fromList (lookupVar inputVariable <> lookupVar outputVariable)
  not $ Set.disjoint referencedVars appVars

makeMetaNetwork :: [NetworkApplicationReplacement] -> MetaNetwork
makeMetaNetwork = fmap $ \NetworkApplicationReplacement {..} ->
  MetaNetworkEntry (fst networkApp) networkInfo

--------------------------------------------------------------------------------
-- Step 4: query assertions

-- | Checks for presence of under-constrained input variables.
checkIfNetworkInputsBounded ::
  (MonadCompile m) =>
  GlobalCtx ->
  QueryFormatID ->
  QueryAddress ->
  [NetworkApplicationReplacement] ->
  ConjunctAll (QueryAssertion NetworkIOElementVariable) ->
  m ()
checkIfNetworkInputsBounded globalCtx queryFormatID queryAddress metaNetworkApps constraints = do
  logCompilerPass MaxDetail "network variable bounds checks" $ do
    let appInputElementVariables app = Tensor.toList $ lookupNetworkElementVariables globalCtx (inputVariable app)
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
compileQueryVariables globalCtx@GlobalCtx {..} compileVariable metaNetworkApps assertions = do
  -- Group network applications
  let f :: NetworkApplicationReplacement -> (Name, [NetworkApplicationReplacement])
      f m = (fst $ networkApp m, [m])
  let networkAppsByName = Map.elems $ Map.fromListWith (<>) (fmap f metaNetworkApps)

  -- Compute the set of new input and output variables
  let initialState = IndexingState mempty mempty
  let compileVarsFn = compileTensorVariable compileVariable globalCtx
  let compileNetworks state apps = foldlM (compileVarsFn (length apps)) state (zip [0 ..] apps)
  indexingState <- foldlM compileNetworks initialState networkAppsByName

  -- Make the queries more asthetically pleasing
  let nameCtx = completeNamedCtx globalCtx
  logDebug MaxDetail $ prettyVerbose (fmap (fst . networkApp) metaNetworkApps)
  let prettifiedAssertions = prettifyQueryContents nameCtx indexingState assertions

  -- Substitute them through the assertions
  let queryVariableMapping = Map.union (networkInputVariables indexingState) (networkOutputVariables indexingState)
  logDebug MaxDetail $ prettyFriendly (WithContext queryVariableMapping nameCtx)
  let substitution = Map.fromList (swap <$> Map.toList queryVariableMapping)
  let newAssertions = fmap (substAssertionVariables nameCtx substitution) prettifiedAssertions

  let variableStore =
        VariableStore
          { queryVariableMapping = queryVariableMapping,
            vehicleVariableCtx = nameCtx,
            userVariables = userTensorVariables
          }
  return (variableStore, newAssertions)

data IndexingState = IndexingState
  { networkInputVariables :: Map QueryVariable NetworkIOElementVariable,
    networkOutputVariables :: Map QueryVariable NetworkIOElementVariable
  }

compileTensorVariable ::
  forall m.
  (MonadCompile m) =>
  CompileQueryVariable ->
  GlobalCtx ->
  Int ->
  IndexingState ->
  (Int, NetworkApplicationReplacement) ->
  m IndexingState
compileTensorVariable compileQueryVar globalCtx totalAppsWithName IndexingState {..} (appIndex, NetworkApplicationReplacement {..}) = do
  inputChildVars <- compileVariables Input inputVariable
  outputChildVars <- compileVariables Output outputVariable
  let merge =
        Map.unionWith
          ( \a b ->
              developerError
                ( "Duplicate compiled variables for"
                    <+> pretty (toLv a)
                    <+> "and"
                    <+> pretty (toLv b)
                )
          )
  return $
    IndexingState
      { networkInputVariables = merge inputChildVars networkInputVariables,
        networkOutputVariables = merge outputChildVars networkOutputVariables
      }
  where
    compileVariables ::
      InputOrOutput ->
      NetworkIOVariable ->
      m (Map QueryVariable NetworkIOElementVariable)
    compileVariables inputOrOutput var = do
      let TensorVariableInfo {..} = lookupTensorVariableInfo var globalCtx
      let childVars = case childrenVariables of
            Nothing -> ZeroDimTensor $ coerce var
            Just (childVariables, _) -> coerce childVariables
      let compileVar = compileQueryVariable inputOrOutput (shapeOf childVars)
      networkQueryVarPairs <- traverse compileVar (Tensor.toList childVars)
      return $ Map.fromList networkQueryVarPairs

    compileQueryVariable ::
      InputOrOutput ->
      TensorShape ->
      NetworkIOElementVariable ->
      m (QueryVariable, NetworkIOElementVariable)
    compileQueryVariable io parentShape var = do
      let varInfo = lookupTensorVariableInfo var globalCtx
      let indices = maybe [] snd (parentVariable varInfo)
      let queryInfo =
            QueryVariableInfo
              { inputOrOutput = io,
                networkName = fst networkApp,
                numberOfNetworkApps = totalAppsWithName,
                networkAppIndex = appIndex,
                parentVariableShape = parentShape,
                parentVariableIndices = indices
              }
      let compiledVar = compileQueryVar queryInfo
      return (compiledVar, var)

substAssertionVariables ::
  CompleteNamedBoundCtx ->
  Map NetworkIOElementVariable QueryVariable ->
  QueryAssertion NetworkIOElementVariable ->
  QueryAssertion QueryVariable
substAssertionVariables nameCtx subst QueryAssertion {..} = do
  let newLHS = fmap (second substVar) lhs
  QueryAssertion {lhs = newLHS, ..}
  where
    substVar :: NetworkIOElementVariable -> QueryVariable
    substVar var = case Map.lookup var subst of
      Just newVar -> newVar
      Nothing ->
        developerError $
          "Malformed network variable substitution. Missing"
            <+> prettyFriendly (WithContext var nameCtx)
            <+> "in"
            <+> prettyFriendly (WithContext (Map.keys subst) nameCtx)

--------------------------------------------------------------------------------
-- Step 5: prettyify assertions

prettifyQueryContents ::
  CompleteNamedBoundCtx ->
  IndexingState ->
  ConjunctAll (QueryAssertion NetworkIOElementVariable) ->
  ConjunctAll (QueryAssertion NetworkIOElementVariable)
prettifyQueryContents ctx indexingState (ConjunctAll conjuncts) = do
  let optimiseFn = optimiseAssertionReadability ctx indexingState
  let optimisedConjuncts = fmap optimiseFn conjuncts
  ConjunctAll $ NonEmpty.sortBy compareAssertion optimisedConjuncts

-- | Applies various optimisations to an assertion to improve readability:
optimiseAssertionReadability ::
  CompleteNamedBoundCtx ->
  IndexingState ->
  QueryAssertion NetworkIOElementVariable ->
  QueryAssertion NetworkIOElementVariable
optimiseAssertionReadability ctx IndexingState {..} (QueryAssertion lhs rel rhs) = do
  let variableList = sort (Map.elems networkInputVariables) <> sort (Map.elems networkOutputVariables)
  let variableIndexMap = Map.fromList $ zip variableList [(0 :: Int) ..]
  let missingVar v = do
        let n = lookupLvInBoundCtx (toLv v) ctx
        developerError $
          "Missing network variable" <+> pretty n <+> "in:"
            <> line
            <> indent 2 (prettyFriendly (WithContext variableList ctx))
  let getIndex v = fromMaybe (missingVar v) $ Map.lookup v variableIndexMap

  -- Sort the assertion by putting:
  --   - positive coefficients before negative ones,
  --   - inputs before outputs
  --   - finally sort by index
  let sortedLHS =
        NonEmpty.sortWith (\(c, v) -> (negate (abs c), getIndex v)) lhs

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
