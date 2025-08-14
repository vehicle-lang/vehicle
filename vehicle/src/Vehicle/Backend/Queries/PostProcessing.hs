{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Backend.Queries.PostProcessing
  ( compilePartitionsToQueries,
  )
where

import Control.Monad (forM, unless, when)
import Control.Monad.Except (MonadError (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Vehicle.Backend.Queries.MetaNetworkCalculation (calculateMetaNetworkApplications)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Error
import Vehicle.Compile.ExpandResources.Core (NetworkContext)
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
import Vehicle.Verify.Specification (CompilationStep (..), QueryMetaData (..), VariableCompilationTrace (..), VariableStore (..), getQueryVariables)
import Vehicle.Verify.Specification.IO (writeVerificationQuery)

--------------------------------------------------------------------------------
-- Main entry point

-- | Converts a set of partitions to a set of individual queries
compilePartitionsToQueries ::
  (MonadCompile m, MonadStdIO m, MonadSupply QueryID m) =>
  GlobalCtx ->
  PropertyMetaData ->
  Partitions TensorVariable ->
  m (MaybeTrivial (DisjunctAll QueryMetaData))
compilePartitionsToQueries ctx metaData partitions = do
  allQueries <- forM (partitionsToDisjuncts partitions) $ \(trace, assertions) -> do
    logCompilerPass MaxDetail "compiling partition" $ do
      let dnfTree = exprToDNF assertions
      forM dnfTree $ \dnfAssertions -> do
        logCompilerPass MaxDetail "compiling potential query" $ do
          eliminationResult <- calculateMetaNetworkApplications ctx dnfAssertions
          case eliminationResult of
            Trivial b -> return $ Trivial b
            NonTrivial (metaNetwork, eliminatedAssertions, eliminationSteps) -> do
              -- Calculate the meta network for the network
              checkIfMetaNetworkSupported metaData (completeNamedCtx ctx) metaNetwork
              reductionResult <- reduceAllRemainingNetworkTensorVariables ctx metaNetwork eliminatedAssertions
              case reductionResult of
                Trivial b -> return $ Trivial b
                NonTrivial (reducedAssertions, reductionSteps) -> do
                  let finalCompilationSteps = reductionSteps <> eliminationSteps <> trace
                  logDebug MaxDetail $ prettyVerbose reductionSteps
                  logDebug MaxDetail $ prettyVerbose eliminationSteps
                  logDebug MaxDetail $ prettyVerbose trace
                  NonTrivial <$> compilePartitionToQuery metaData ctx metaNetwork finalCompilationSteps reducedAssertions
  return $ eliminateTrivialDisjunctions $ disjunctDisjuncts allQueries

compilePartitionToQuery ::
  (MonadCompile m, MonadStdIO m, MonadSupply QueryID m) =>
  PropertyMetaData ->
  GlobalCtx ->
  NetworkApplicationReplacements ->
  [CompilationStep] ->
  ConjunctAll (Assertion NetworkIOElementVariable) ->
  m QueryMetaData
compilePartitionToQuery PropertyMetaData {..} ctx metaNetworkApps compilationSteps assertions = do
  -- Calculate query address
  queryID <- demand
  let queryAddress = (propertyAddress, queryID)

  logCompilerPass MaxDetail ("compiling query" <+> pretty queryID) $ do
    linearisedAssertions <- traverse lineariseAssertions assertions

    -- Check if all variables have lower and upper bounds
    checkIfNetworkInputsBounded ctx queryFormat queryAddress metaNetworkApps linearisedAssertions

    -- Convert to query variables
    (variableStore, queryAssertions) <-
      compileQueryVariables ctx (compileVariable queryFormat) metaNetworkApps linearisedAssertions

    -- Construct the meta-data object
    let metaNetwork = makeMetaNetwork networkCtx metaNetworkApps
    let reconstruction = Reconstruction compilationSteps
    let queryMetaData = QueryMetaData queryAddress metaNetwork variableStore reconstruction

    -- Actually compile to the query to the format
    queryText <- compileQuery queryFormat queryAddress metaNetwork (getQueryVariables variableStore) queryAssertions

    -- Write out the query to disk
    case outputLocation of
      Nothing -> programOutput $ line <> line <> pretty queryAddress <> line <> pretty queryText
      Just folder -> writeVerificationQuery queryFormat folder (queryMetaData, queryText)

    return queryMetaData

--------------------------------------------------------------------------------
-- Step 1: Reduce any equalities over multi-dimensional tensors to equalities
-- over zero-dimensional tensors (i.e. rationals).

reduceAllRemainingNetworkTensorVariables ::
  forall m.
  (MonadCompile m) =>
  GlobalCtx ->
  NetworkApplicationReplacements ->
  ConjunctAll (Assertion TensorVariable) ->
  m (MaybeTrivial (ConjunctAll (Assertion NetworkIOElementVariable), [CompilationStep]))
reduceAllRemainingNetworkTensorVariables ctx metaNetwork assertions = do
  logCompilerPass MaxDetail "eliminating remaining tensor assertions" $ do
    -- Create the assertions
    let convertedAssertions = fmap (convert ctx) assertions
    let maybeNewAssertions = concatConjuncts <$> eliminateTrivialConjunctions convertedAssertions
    case maybeNewAssertions of
      Trivial b -> return $ Trivial b
      NonTrivial newAssertions -> do
        -- Update the compilation trace
        -- (Note that we could be more precise about which IO variables we actually use here.)
        let ioVariables = concatMap (\(_, app) -> [inputVariable app, outputVariable app]) $ toListOfApplications metaNetwork
        let findElementVars = lookupNetworkElementVariables ctx
        let mkStep var = ReconstructTensorVariable (coerce var) (findElementVars var)
        let newSteps = mkStep <$> ioVariables

        return $ NonTrivial (newAssertions, newSteps)

convert ::
  GlobalCtx ->
  Assertion TensorVariable ->
  MaybeTrivial (ConjunctAll (Assertion NetworkIOElementVariable))
convert ctx (NormalisedRelation relation linearExpr)
  | isZeroDimensional linearExpr = do
      -- TODO maybe we should actually check here rather than just casting
      let castExpr = mapVariables coerce linearExpr
      NonTrivial $ ConjunctAll (NormalisedRelation relation castExpr :| [])
  | otherwise = do
      let rationalEqualities = reduceTensorExpr ctx linearExpr
      let reducedAssertions = fmap (NormalisedRelation relation) rationalEqualities
      let finalAssertions = fmap (convert ctx) reducedAssertions
      case finalAssertions of
        [] -> Trivial True
        (v : vs) -> fmap concatConjuncts $ eliminateTrivialConjunctions $ ConjunctAll (v :| vs)

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

makeMetaNetwork :: NetworkContext -> NetworkApplicationReplacements -> MetaNetwork
makeMetaNetwork networkCtx metaNetworkApps = do
  let networkNames = fst <$> toListOfApplications metaNetworkApps
  let missing name = developerError $ "missing network" <+> quotePretty name <+> "in context."
  let toEntry name = MetaNetworkEntry name (fromMaybe (missing name) $ Map.lookup name networkCtx)
  fmap toEntry networkNames

--------------------------------------------------------------------------------
-- Step 4: query assertions

-- | Check if the query format supports the current meta-network configuration
checkIfMetaNetworkSupported ::
  (MonadCompile m) =>
  PropertyMetaData ->
  CompleteNamedBoundCtx ->
  NetworkApplicationReplacements ->
  m ()
checkIfMetaNetworkSupported PropertyMetaData {..} nameCtx metaNetworkApps
  | supportsMultipleNetworks queryFormat = return ()
  | otherwise = do
      case toListOfApplications metaNetworkApps of
        [] -> developerError "Empty"
        [_app] -> return ()
        apps -> do
          let formatID = queryFormatID queryFormat
          let appsWithValues = fmap (second inputValue) apps
          throwError $ UnsupportedMultipleNetworkApplications formatID propertyProvenance nameCtx appsWithValues

-- | Checks for presence of under-constrained input variables.
checkIfNetworkInputsBounded ::
  (MonadCompile m) =>
  GlobalCtx ->
  QueryFormat ->
  QueryAddress ->
  NetworkApplicationReplacements ->
  ConjunctAll (QueryAssertion NetworkIOElementVariable) ->
  m ()
checkIfNetworkInputsBounded globalCtx queryFormat queryAddress metaNetworkApps constraints = do
  logCompilerPass MaxDetail "network variable bounds checks" $ do
    let listOfApps = toListOfApplications metaNetworkApps

    let appInputElementVariables (_name, app) = Tensor.toList $ coerce $ lookupNetworkElementVariables globalCtx (inputVariable app)
    let allInputElementVariables = concatMap appInputElementVariables listOfApps

    finalStatuses <- variableConstraintStatus allInputElementVariables constraints

    -- If Marabou, then warn if all inputs are constant.
    -- See https://github.com/NeuralNetworkVerification/Marabou/issues/670
    let formatID = queryFormatID queryFormat
    when (queryFormatID queryFormat == MarabouQueries && all (== Constant) finalStatuses) $
      logWarning $
        AllConstantNetworkInputVars formatID queryAddress

    -- Check if all inputs are well-specified.
    let unboundedVariables = Map.toList $ Map.mapMaybe toUnderConstrainedStatus finalStatuses
    unless (null unboundedVariables) $ do
      let lookupVar v = variableName $ lookupTensorVariableInfo globalCtx v
      let unboundedVariableNames = fmap (first lookupVar) unboundedVariables
      logWarning $
        UnboundedNetworkInputVariables formatID queryAddress unboundedVariableNames

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

data IndexingState = IndexingState
  { networkInputVariables :: [(QueryVariable, NetworkIOElementVariable)],
    networkOutputVariables :: [(QueryVariable, NetworkIOElementVariable)]
  }

compileQueryVariables ::
  forall m.
  (MonadCompile m) =>
  GlobalCtx ->
  CompileQueryVariable ->
  NetworkApplicationReplacements ->
  ConjunctAll (QueryAssertion NetworkIOElementVariable) ->
  m (VariableStore, ConjunctAll (QueryAssertion QueryVariable))
compileQueryVariables globalCtx@GlobalCtx {..} compileVariable metaNetworkApps assertions = do
  -- Compute the set of new input and output variables
  let initialState = IndexingState mempty mempty
  indexingState <- foldlM compileNetworkApplicationsVariables initialState (Map.toList metaNetworkApps)

  -- Make the queries more asthetically pleasing
  let nameCtx = completeNamedCtx globalCtx
  let prettifiedAssertions = prettifyQueryContents nameCtx indexingState assertions

  -- Substitute them through the assertions
  let queryVariableMapping = Map.fromList (networkInputVariables indexingState <> networkOutputVariables indexingState)
  let substitution = Map.fromList (swap <$> Map.toList queryVariableMapping)
  let newAssertions = fmap (substAssertionVariables nameCtx substitution) prettifiedAssertions

  let variableStore =
        VariableStore
          { queryVariableMapping = queryVariableMapping,
            vehicleVariableCtx = nameCtx,
            userVariables = userTensorVariables
          }
  return (variableStore, newAssertions)
  where
    compileNetworkApplicationsVariables ::
      IndexingState ->
      (Name, NonEmpty NetworkApplicationInfo) ->
      m IndexingState
    compileNetworkApplicationsVariables state (networkName, applications) = do
      let compileApp = compileNetworkApplicationVariables networkName (length applications)
      foldlM compileApp state (zip [0 ..] $ NonEmpty.toList applications)

    compileNetworkApplicationVariables ::
      Name ->
      Int ->
      IndexingState ->
      (Int, NetworkApplicationInfo) ->
      m IndexingState
    compileNetworkApplicationVariables networkName totalAppsWithName IndexingState {..} (appIndex, NetworkApplicationInfo {..}) = do
      inputChildVars <- compileTensorVariables networkName (appIndex, totalAppsWithName) Input inputVariable
      outputChildVars <- compileTensorVariables networkName (appIndex, totalAppsWithName) Output outputVariable
      return $
        IndexingState
          { networkInputVariables = inputChildVars <> networkInputVariables,
            networkOutputVariables = outputChildVars <> networkOutputVariables
          }

    compileTensorVariables ::
      Name ->
      (Int, Int) ->
      InputOrOutput ->
      NetworkIOVariable ->
      m [(QueryVariable, NetworkIOElementVariable)]
    compileTensorVariables networkName appIndex inputOrOutput var = do
      let TensorVariableInfo {..} = lookupTensorVariableInfo globalCtx var
      let childVars = case childrenVariables of
            Nothing -> ZeroDimTensor $ coerce var
            Just (childVariables, _) -> coerce childVariables
      let compileVar = compileQueryVariable networkName appIndex inputOrOutput (shapeOf childVars)
      traverse compileVar (Tensor.toList childVars)

    compileQueryVariable ::
      Name ->
      (Int, Int) ->
      InputOrOutput ->
      TensorShape ->
      NetworkIOElementVariable ->
      m (QueryVariable, NetworkIOElementVariable)
    compileQueryVariable networkName (appIndex, totalAppsWithName) io parentShape var = do
      let varInfo = lookupTensorVariableInfo globalCtx var
      let indices = maybe [] snd (parentVariable varInfo)
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
            <+> "in:"
            <> lineIndent (prettyFriendly (WithContext (Map.keys subst) nameCtx))

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
  let variableList = sort (fmap snd networkInputVariables) <> sort (fmap snd networkOutputVariables)
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
