{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.Solver.QueryCompilation
  ( compilePartitionsToQueries,
  )
where

import Control.Monad (forM)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Vehicle.Backend.Solver.QueryCompilation.InputBoundsCheck
import Vehicle.Backend.Solver.QueryCompilation.MetaNetworkCalculation (calculateMetaNetworkApplications)
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (NetworkContext)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Tensor as Tensor
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Bound.Tensor
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface
import Vehicle.Verify.Specification (CompilationStep (..), QueryMetaData (..), ReconstructionDepth (AllDimensions), VariableCompilationTrace (..), VariableStore (..), getQueryVariables)
import Vehicle.Verify.Specification.IO (writeVerificationQuery)

--------------------------------------------------------------------------------
-- Main entry point

-- | Converts a set of partitions to a set of individual queries
compilePartitionsToQueries ::
  (MonadCompile m, MonadStdIO m, MonadSupply QueryID m) =>
  PropertyMetaData ->
  GlobalCtx ->
  Partitions ->
  m (MaybeTrivial (DisjunctAll QueryMetaData))
compilePartitionsToQueries metaData ctx partitions = do
  allQueries <- forM (partitionsToDisjuncts partitions) $ \(trace, assertions) -> do
    logCompilerSection2 MaxDetail "compiling partition" $ do
      let dnfTree = exprToDNF assertions
      logDebug MaxDetail $ "Conversion to DNF resulted in" <+> pretty (length dnfTree) <+> "potential queries"
      forM dnfTree $ \dnfAssertions -> do
        logCompilerSection2 MaxDetail "compiling potential query" $
          runMaybeTrivialT $ do
            (metaNetwork, eliminatedAssertions, eliminationSteps) <- calculateMetaNetworkApplications metaData ctx dnfAssertions
            -- Check if all variables have lower and upper bounds
            boundedAssertions <- findInputVariableBounds metaData ctx metaNetwork eliminatedAssertions
            (reducedAssertions, reductionSteps) <- reduceAllRemainingNetworkTensorVariables ctx metaNetwork boundedAssertions
            let finalCompilationSteps = reductionSteps <> eliminationSteps <> trace
            result <- compilePartitionToQuery metaData ctx metaNetwork finalCompilationSteps reducedAssertions
            nonTrivial result

  return $ eliminateTrivialDisjunctions $ disjunctDisjuncts allQueries

compilePartitionToQuery ::
  (MonadCompile m, MonadStdIO m, MonadSupply QueryID m) =>
  PropertyMetaData ->
  GlobalCtx ->
  NetworkApplications ->
  [CompilationStep] ->
  BoundedAssertions NetworkIOElementVariable NetworkIOElementVariable Rational ->
  m QueryMetaData
compilePartitionToQuery PropertyMetaData {..} ctx metaNetworkApps compilationSteps (BoundedAssertions bounds assertions) = do
  -- Calculate query address
  queryID <- demand
  let queryAddress = (propertyAddress, queryID)

  logCompilerSection2 MaxDetail ("compiling query" <+> pretty queryID) $ do
    -- Create the substitution from network variables to query variables
    (variableSubstitution, variableStore) <- compileQueryVariables ctx (compileVariable queryFormat) metaNetworkApps

    -- Construct the meta-data object
    let metaNetwork = makeMetaNetwork networkCtx metaNetworkApps
    let reconstruction = Reconstruction compilationSteps
    let queryMetaData = QueryMetaData queryAddress metaNetwork variableStore reconstruction

    -- Convert the compiled bounds and assertions to the format expected by the verifier interface
    let nameCtx = completeNamedCtx ctx
    let queryBounds = substBounds nameCtx variableSubstitution bounds
    queryAssertions <- substAssertions nameCtx variableSubstitution assertions

    -- Actually compile to the query to the format
    queryText <- compileQuery queryFormat queryAddress metaNetwork (getQueryVariables variableStore) queryBounds queryAssertions

    -- Write out the query to disk
    case outputLocation of
      Nothing -> programOutput $ line <> line <> pretty queryAddress <> line <> pretty queryText
      Just folder -> writeVerificationQuery queryFormat folder (queryMetaData, queryText)

    return queryMetaData

makeMetaNetwork :: NetworkContext -> NetworkApplications -> MetaNetwork
makeMetaNetwork networkCtx metaNetworkApps = do
  let networkNames = Map.toList metaNetworkApps
  let missing name = developerError $ "missing network" <+> quotePretty name <+> "in context."
  let lookupInfo name = fromMaybe (missing name) $ Map.lookup name networkCtx
  let toEntry (name, apps) = (name, lookupInfo name, length apps)
  fmap toEntry networkNames

--------------------------------------------------------------------------------
-- Step 1: Reduce any equalities over multi-dimensional tensors to equalities
-- over zero-dimensional tensors (i.e. rationals).

reduceAllRemainingNetworkTensorVariables ::
  forall m.
  (MonadCompile m, MonadMaybeTrivial m) =>
  GlobalCtx ->
  NetworkApplications ->
  BoundedAssertions NetworkInputTensorVariable SliceVariable RatTensor ->
  m (BoundedAssertions NetworkIOElementVariable NetworkIOElementVariable Rational, [CompilationStep])
reduceAllRemainingNetworkTensorVariables ctx metaNetwork (BoundedAssertions bounds assertions) = do
  logCompilerSection2 MaxDetail "eliminating remaining tensor assertions" $ do
    -- Create the assertions
    let newBounds = Map.fromList $ concatMap (reduceBound ctx) $ Map.toList bounds
    let flattenedAssertions = concatMap (reduceAssertion ctx) assertions

    -- Update the compilation trace
    -- (Note that we could be more precise about which IO variables we actually use here.)
    let ioVariables = concatMap (\(_, app) -> [toTensorVar $ inputVariable app, toTensorVar $ outputVariable app]) $ toListOfApplications metaNetwork
    let nestedVar = lookupTensorVariable (globalBoundVarCtx ctx)
    let mkStep var = ReconstructTensorVariable (nestedVar var) AllDimensions
    let newSteps = mkStep <$> ioVariables

    case flattenedAssertions of
      [] -> trivial True
      a : as -> nonTrivial $ do
        let newAssertions = ConjunctAll (a :| as)
        let newBoundedAssertions = BoundedAssertions newBounds newAssertions
        (newBoundedAssertions, newSteps)

reduceBound ::
  GlobalCtx ->
  (NetworkInputTensorVariable, (RatTensor, RatTensor)) ->
  [(NetworkIOElementVariable, (Rational, Rational))]
reduceBound ctx (inputTensorVar, bounds) = go (toSliceVar inputTensorVar, bounds)
  where
    go ::
      (SliceVariable, (RatTensor, RatTensor)) ->
      [(NetworkIOElementVariable, (Rational, Rational))]
    go (var, (lowerBound, upperBound)) =
      case shapeOf lowerBound of
        [] -> do
          let elementVar = coerce var
          let lowerValue = extractRationalConstant lowerBound
          let upperValue = extractRationalConstant upperBound
          [(elementVar, (lowerValue, upperValue))]
        _ : _ -> do
          let childVars = lookupChildVariablesCertain ctx var
          let lowerBounds = unstack lowerBound
          let upperBounds = unstack upperBound
          concat $ zipWith3 (\v l u -> go (v, (l, u))) childVars lowerBounds upperBounds

extractRationalConstant :: RatTensor -> Rational
extractRationalConstant = \case
  ZeroDimTensor v -> v
  t -> developerError $ "Cannot extract constant from multi-dim tensor" <+> pretty t

reduceAssertion ::
  GlobalCtx ->
  LinearAssertion ->
  [Assertion (LinearExpr NetworkIOElementVariable Rational)]
reduceAssertion ctx (NormalisedRelation relation linearExpr) =
  case shapeOf linearExpr of
    [] -> do
      -- TODO maybe we should actually check here rather than just casting
      let castExpr = mapExpr coerce extractRationalConstant linearExpr
      [NormalisedRelation relation castExpr]
    dim : _ -> do
      let rationalEqualities = reduceTensorExpr dim (lookupChildVariablesCertain ctx) linearExpr
      let reducedAssertions = fmap (NormalisedRelation relation) rationalEqualities
      concatMap (reduceAssertion ctx) reducedAssertions

--------------------------------------------------------------------------------
-- Step 5: compiling query variables

data IndexingState = IndexingState
  { networkInputVariables :: [(QueryVariable, NetworkIOElementVariable)],
    networkOutputVariables :: [(QueryVariable, NetworkIOElementVariable)]
  }

compileQueryVariables ::
  forall m.
  (MonadCompile m) =>
  GlobalCtx ->
  CompileQueryVariable ->
  NetworkApplications ->
  m (Map NetworkIOElementVariable QueryVariable, VariableStore)
compileQueryVariables globalCtx@GlobalCtx {..} compileVariable metaNetworkApps = do
  -- Compute the set of new input and output variables
  let initialState = IndexingState mempty mempty
  indexingState <- foldlM compileNetworkApplicationsVariables initialState (Map.toList metaNetworkApps)

  -- Make the queries more asthetically pleasing
  let nameCtx = completeNamedCtx globalCtx

  -- Substitute them through the assertions
  let queryVariableMapping = Map.fromList (networkInputVariables indexingState <> networkOutputVariables indexingState)
  let substitution = Map.fromList (swap <$> Map.toList queryVariableMapping)

  let variableStore =
        VariableStore
          { queryVariableMapping = queryVariableMapping,
            vehicleVariableCtx = nameCtx,
            userVariables = userTensorVariables
          }
  return (substitution, variableStore)
  where
    compileNetworkApplicationsVariables ::
      IndexingState ->
      (Name, NonEmpty NetworkApplicationInfo) ->
      m IndexingState
    compileNetworkApplicationsVariables state (networkName, applications) = do
      let compileApp = compileNetworkApplicationVariables networkName (length applications)
      foldlM compileApp state (zip [1 ..] $ NonEmpty.toList applications)

    compileNetworkApplicationVariables ::
      Name ->
      Int ->
      IndexingState ->
      (Int, NetworkApplicationInfo) ->
      m IndexingState
    compileNetworkApplicationVariables networkName totalAppsWithName IndexingState {..} (appIndex, NetworkApplicationInfo {..}) = do
      inputChildVars <- compileTensorVariables networkName (appIndex, totalAppsWithName) Input (toTensorVar inputVariable)
      outputChildVars <- compileTensorVariables networkName (appIndex, totalAppsWithName) Output (toTensorVar outputVariable)
      return $
        IndexingState
          { networkInputVariables = inputChildVars <> networkInputVariables,
            networkOutputVariables = outputChildVars <> networkOutputVariables
          }

    compileTensorVariables ::
      Name ->
      (Int, Int) ->
      InputOrOutput ->
      TensorVariable ->
      m [(QueryVariable, NetworkIOElementVariable)]
    compileTensorVariables networkName appIndex inputOrOutput var = do
      let nestedVar = lookupTensorVariable globalBoundVarCtx var
      let compileVar = compileQueryVariable networkName appIndex inputOrOutput (shapeOf nestedVar)
      traverse compileVar (elementVariablesOf nestedVar)

    compileQueryVariable ::
      Name ->
      (Int, Int) ->
      InputOrOutput ->
      TensorShape ->
      (NetworkIOElementVariable, TensorIndices) ->
      m (QueryVariable, NetworkIOElementVariable)
    compileQueryVariable networkName (appIndex, totalAppsWithName) io parentShape (var, indices) = do
      let queryInfo =
            QueryVariableInfo
              { networkName = networkName,
                inputOrOutput = io,
                numberOfNetworkApps = totalAppsWithName,
                networkAppIndex = appIndex,
                parentVariableShape = parentShape,
                parentVariableIndices = indices
              }
      return (compileVariable queryInfo, var)

--------------------------------------------------------------------------------
-- Step 6: compiling query variables

substBounds ::
  CompleteNamedBoundCtx ->
  Map NetworkIOElementVariable QueryVariable ->
  Map NetworkIOElementVariable (Rational, Rational) ->
  QueryVariableBounds
substBounds ctx subst bounds = first (substVar ctx subst) <$> Map.toList bounds

substAssertions ::
  (MonadCompile m) =>
  CompleteNamedBoundCtx ->
  Map NetworkIOElementVariable QueryVariable ->
  ConjunctAll (Assertion (LinearExpr NetworkIOElementVariable Rational)) ->
  m (ConjunctAll (QueryAssertion QueryVariable))
substAssertions nameCtx substitution assertions = do
  let optimisedConjuncts = unConjunctAll $ fmap optimiseAssertionReadability assertions
  let prettifiedAssertions = ConjunctAll $ NonEmpty.sortBy compareAssertion optimisedConjuncts
  let newAssertions = fmap (substAssertionVariables nameCtx substitution) prettifiedAssertions
  return newAssertions

substAssertionVariables ::
  CompleteNamedBoundCtx ->
  Map NetworkIOElementVariable QueryVariable ->
  QueryAssertion NetworkIOElementVariable ->
  QueryAssertion QueryVariable
substAssertionVariables nameCtx subst QueryAssertion {..} = do
  let newLHS = fmap (second (substVar nameCtx subst)) lhs
  QueryAssertion {lhs = newLHS, ..}

substVar :: CompleteNamedBoundCtx -> Map NetworkIOElementVariable QueryVariable -> NetworkIOElementVariable -> QueryVariable
substVar nameCtx subst var = case Map.lookup var subst of
  Just newVar -> newVar
  Nothing ->
    developerError $
      "Malformed network variable substitution. Missing"
        <+> prettyFriendly (WithContext var nameCtx)
        <+> "in:"
        <> lineIndent (prettyFriendly (WithContext (Map.keys subst) nameCtx))

-- | Applies various optimisations to an assertion to improve readability:
optimiseAssertionReadability ::
  Assertion (LinearExpr NetworkIOElementVariable Rational) ->
  QueryAssertion NetworkIOElementVariable
optimiseAssertionReadability (NormalisedRelation relation (Sparse coefficients constant)) = do
  -- Create the relation
  let rel = relationToQueryRelation relation

  -- Create the LHS
  let rationalVarCoefs = swap <$> Map.toList coefficients
  let lhs = case rationalVarCoefs of
        (c : cs) -> c :| cs
        [] -> developerError "Found trivial assertion"
  let sortedLHS = NonEmpty.sortWith (\(c, v) -> (negate (abs c), v)) lhs

  -- Create the RHS
  let rhs = -constant

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
