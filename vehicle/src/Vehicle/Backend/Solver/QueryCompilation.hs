{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.Solver.QueryCompilation
  ( compilePartitionsToQueries,
  )
where

import Control.Monad (forM, zipWithM)
import Control.Monad.Reader (MonadReader (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Vector.Internal.Check (HasCallStack)
import Vehicle.Backend.Solver.QueryCompilation.Core
import Vehicle.Backend.Solver.QueryCompilation.InputBoundsCheck
import Vehicle.Backend.Solver.QueryCompilation.MetaNetworkCalculation (calculateMetaNetworkApplications)
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (NetworkContext)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion
import Vehicle.Data.Bound (BoundedValue (..), Domain (..), unstackBounds)
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.LinearExpr
import Vehicle.Data.MaybeTrivial
import Vehicle.Data.Tensor as Tensor
import Vehicle.Data.Variable.Bound.Context.Name
import Vehicle.Data.Variable.Bound.Context.Tensor.Class
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Interface
import Vehicle.Verify.Specification (CompilationStep (..), QueryMetaData (..), ReconstructionDepth (AllDimensions), VariableCompilationTrace (..), VariableStore (..), getQueryVariables)
import Vehicle.Verify.Specification.IO (writeVerificationQuery)

--------------------------------------------------------------------------------
-- Main entry point

-- | Converts a set of partitions to a set of individual queries
compilePartitionsToQueries ::
  (MonadQueryCompilation m, MonadStdIO m, MonadSupply QueryID m) =>
  Partitions ->
  m (MaybeTrivial (DisjunctAll QueryMetaData))
compilePartitionsToQueries partitions = do
  allQueries <- forM (partitionsToDisjuncts partitions) $ \(trace, assertions) -> do
    logCompilerSection2 MaxDetail "compiling partition" $ do
      let dnfTree = exprToDNF assertions
      logDebug MaxDetail $ "Conversion to DNF resulted in" <+> pretty (length dnfTree) <+> "potential queries"
      forM dnfTree $ \dnfAssertions -> do
        logCompilerSection2 MaxDetail "compiling potential query" $
          runMaybeTrivialT $ do
            (metaNetwork, eliminatedAssertions, eliminationSteps) <- calculateMetaNetworkApplications dnfAssertions
            -- Check if all variables have lower and upper bounds
            boundedAssertions <- findInputVariableBounds metaNetwork eliminatedAssertions
            (reducedAssertions, reductionSteps) <- reduceAllRemainingNetworkTensorVariables metaNetwork boundedAssertions
            let finalCompilationSteps = reductionSteps <> eliminationSteps <> trace
            result <- compilePartitionToQuery metaNetwork finalCompilationSteps reducedAssertions
            nonTrivial result

  return $ eliminateTrivialDisjunctions $ disjunctDisjuncts allQueries

compilePartitionToQuery ::
  (MonadQueryCompilation m, MonadStdIO m, MonadSupply QueryID m) =>
  NetworkApplications ->
  [CompilationStep] ->
  BoundedAssertions NetworkIOElementVariable NetworkIOElementVariable Rational ->
  m QueryMetaData
compilePartitionToQuery metaNetworkApps compilationSteps (BoundedAssertions bounds assertions) = do
  (PropertyMetaData {..}, _) <- ask

  -- Calculate query address
  queryID <- demand
  let queryAddress = QueryAddress propertyAddress queryID

  logCompilerSection2 MaxDetail ("compiling query" <+> pretty queryID) $ do
    -- Create the substitution from network variables to query variables
    (variableSubstitution, variableStore) <- compileQueryVariables (compileVariable queryFormat) metaNetworkApps

    -- Construct the meta-data object
    let metaNetwork = makeMetaNetwork networkCtx metaNetworkApps
    let reconstruction = Reconstruction compilationSteps
    let queryMetaData = QueryMetaData queryAddress metaNetwork variableStore reconstruction

    -- Convert the compiled bounds and assertions to the format expected by the solver interface
    nameCtx <- getCompleteNamedCtx
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
  (MonadQueryCompilation m, MonadMaybeTrivial m) =>
  NetworkApplications ->
  BoundedAssertions NetworkInputTensorVariable SliceVariable RatTensor ->
  m (BoundedAssertions NetworkIOElementVariable NetworkIOElementVariable Rational, [CompilationStep])
reduceAllRemainingNetworkTensorVariables metaNetwork (BoundedAssertions bounds assertions) = do
  logCompilerSection2 MaxDetail "eliminating remaining tensor assertions" $ do
    -- Create the assertions
    newBounds <- concat <$> traverse reduceDomain bounds
    flattenedAssertions <- concat <$> traverse reduceAssertion assertions

    -- Update the compilation trace
    -- (Note that we could be more precise about which IO variables we actually use here.)
    let toIOVar (_, app) = [toTensorVar $ inputVariable app, toTensorVar $ outputVariable app]
    let ioVariables = concatMap toIOVar $ toListOfApplications metaNetwork
    newSteps <- forM ioVariables $ \var -> do
      nestedVar <- lookupNestedTensorVariable var
      return $ ReconstructTensorVariable nestedVar AllDimensions

    case flattenedAssertions of
      [] -> trivial True
      a : as -> nonTrivial $ do
        let newAssertions = ConjunctAll (a :| as)
        let newBoundedAssertions = BoundedAssertions newBounds newAssertions
        (newBoundedAssertions, newSteps)

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
          lowerBounds <- unstackBounds lowerBound
          upperBounds <- unstackBounds upperBound
          let domains = zipWith Domain lowerBounds upperBounds
          concat <$> zipWithM (curry go) childVars domains

extractRationalConstant :: (Pretty a) => Tensor a -> a
extractRationalConstant = \case
  ZeroDimTensor v -> v
  t -> developerError $ "Cannot extract constant from multi-dim tensor" <+> pretty t

reduceAssertion ::
  (MonadQueryCompilation m) =>
  LinearAssertion ->
  m [Assertion (LinearExpr NetworkIOElementVariable Rational)]
reduceAssertion (NormalisedRelation relation linearExpr) =
  case shapeOf linearExpr of
    [] -> do
      -- TODO maybe we should actually check here rather than just casting
      let castExpr = mapExpr coerce extractRationalConstant linearExpr
      return [NormalisedRelation relation castExpr]
    dim : _ -> do
      rationalEqualities <- reduceTensorExpr dim lookupChildVariablesCertain linearExpr
      let reducedAssertions = fmap (NormalisedRelation relation) rationalEqualities
      concat <$> traverse reduceAssertion reducedAssertions

--------------------------------------------------------------------------------
-- Step 5: compiling query variables

data IndexingState = IndexingState
  { networkInputVariables :: [(QueryVariable, NetworkIOElementVariable)],
    networkOutputVariables :: [(QueryVariable, NetworkIOElementVariable)]
  }

compileQueryVariables ::
  forall m.
  (MonadCompile m, MonadQueryCompilation m) =>
  CompileQueryVariable ->
  NetworkApplications ->
  m (Map NetworkIOElementVariable QueryVariable, VariableStore)
compileQueryVariables compileVariable metaNetworkApps = do
  -- Compute the set of new input and output variables
  let initialState = IndexingState mempty mempty
  indexingState <- foldlM compileNetworkApplicationsVariables initialState (Map.toList metaNetworkApps)

  -- Make the queries more asthetically pleasing
  nameCtx <- getCompleteNamedCtx
  -- Substitute them through the assertions
  let queryVariableMapping = Map.fromList (networkInputVariables indexingState <> networkOutputVariables indexingState)
  let substitution = Map.fromList (swap <$> Map.toList queryVariableMapping)

  (_, GlobalCtx {..}) <- ask
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
      nestedVar <- lookupNestedTensorVariable var
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
  [BoundedValue NetworkIOElementVariable (Domain Rational)] ->
  QueryVariableBounds
substBounds ctx subst = fmap (first (substVar ctx subst))

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

substVar :: (HasCallStack) => CompleteNamedBoundCtx -> Map NetworkIOElementVariable QueryVariable -> NetworkIOElementVariable -> QueryVariable
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
