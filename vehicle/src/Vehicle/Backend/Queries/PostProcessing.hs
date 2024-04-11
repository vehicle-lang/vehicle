module Vehicle.Backend.Queries.PostProcessing
  ( convertPartitionsToQueries,
    compileQueryToFormat,
  )
where

import Control.DeepSeq (force)
import Control.Monad (foldM, forM, unless, when)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (get)
import Data.Either (partitionEithers)
import Data.HashMap.Strict qualified as HashMap (toList)
import Data.LinkedHashMap qualified as LinkedHashMap
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.BooleanExpr
import Vehicle.Data.LinearExpr
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Syntax.Builtin
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat (QueryFormat (..), QueryFormatID (..), queryFormatID)
import Vehicle.Verify.Specification (QueryMetaData (QueryMetaData), queryAddress)
import Vehicle.Verify.Specification.IO (writeVerificationQuery)
import Vehicle.Verify.Variable

--------------------------------------------------------------------------------
-- Main entry point

convertPartitionsToQueries ::
  (MonadQueryStructure m, MonadStdIO m) =>
  Partitions ->
  m (DisjunctAll QueryMetaData)
convertPartitionsToQueries partitions = do
  PropertyMetaData {..} <- ask
  allQueries <- forM (partitionsToDisjuncts partitions) $ \(reconstruction, assertionTree) -> do
    fullReconstruction <- reconstructNetworkTensorVars reconstruction
    networkVarAssertions <- convertToNetworkRatVarAssertions assertionTree
    let dnfTree = exprToDNF networkVarAssertions
    forM dnfTree $ \conjuncts -> do
      -- Calculate meta network
      (metaNetwork, newConjuncts) <- calculateMetaNetwork conjuncts
      -- Compile queries to particular format
      let contents = prettifyQueryContents (variables metaNetwork) newConjuncts
      -- Compile to query format
      (queryMetaData, queryText) <- compileQueryToFormat (metaNetwork, fullReconstruction, contents)

      -- Write out the query
      case outputLocation of
        Nothing -> programOutput $ line <> line <> pretty (queryAddress queryMetaData) <> line <> pretty queryText
        Just folder -> writeVerificationQuery queryFormat folder (queryMetaData, queryText)

      return queryMetaData
  return $ disjunctDisjuncts allQueries

-- This is separated from `convertPartitionsToQueries` above because for
-- performance reasons we don't want `MonadSupply` to be needed everywhere.
compileQueryToFormat ::
  (MonadQueryStructure m) =>
  (MetaNetwork, UserVariableReconstruction, QueryContents) ->
  m (QueryMetaData, QueryText)
compileQueryToFormat (metaNetwork, userVars, contents@QueryContents {..}) = do
  -- Calculate query address
  PropertyMetaData {..} <- ask
  queryID <- getAndUpdateQueryID
  let queryAddress = (propertyAddress, queryID)
  checkIfNetworkInputsBounded queryAddress metaNetwork queryAssertions
  let queryMetaData = QueryMetaData queryAddress metaNetwork userVars
  queryText <- formatQuery queryFormat queryAddress contents
  return (queryMetaData, queryText)

--------------------------------------------------------------------------------
-- Step 0: Add reconstruction steps for network tensor variables.

reconstructNetworkTensorVars ::
  (MonadQueryStructure m) =>
  UserVariableReconstruction ->
  m UserVariableReconstruction
reconstructNetworkTensorVars solutions = do
  GlobalCtx {..} <- get
  let networkTensorVars = sortOn fst $ HashMap.toList networkVariableReductions
  let mkStep (var, (ratVars, _)) = ReconstructTensor (NetworkTensorVar var) (fmap NetworkRationalVar ratVars)
  return $ foldr (\v -> (mkStep v :)) solutions networkTensorVars

--------------------------------------------------------------------------------
-- Step 1: Reduce tensor equalities to a series of rational equalities and
-- checks that the expression only contains network variables.

convertToNetworkRatVarAssertions ::
  forall m.
  (MonadQueryStructure m) =>
  AssertionTree ->
  m (BooleanExpr QueryAssertion)
convertToNetworkRatVarAssertions = go
  where
    go :: BooleanExpr Assertion -> m (BooleanExpr QueryAssertion)
    go = \case
      Query x -> convert x
      Disjunct xs -> Disjunct <$> traverse go xs
      Conjunct xs -> Conjunct <$> traverse go xs

    convert :: Assertion -> m (BooleanExpr QueryAssertion)
    convert = \case
      TensorEq (TensorEquality tensorEquality) -> do
        rationalEqualities <- reduceTensorExpr tensorEquality
        let assertions = fmap (Query . RationalEq . RationalEquality) rationalEqualities
        go $ Conjunct $ ConjunctAll (NonEmpty.fromList assertions)
      RationalEq (RationalEquality expr) ->
        Query <$> makeQueryAssertion Equal expr
      RationalIneq (RationalInequality strict expr) -> do
        let rel = if strict == Strict then LessThan else LessThanOrEqual
        Query <$> makeQueryAssertion rel expr

makeQueryAssertion ::
  (MonadCompile m) =>
  Relation ->
  LinearExpr RationalVariable Rational ->
  m QueryAssertion
makeQueryAssertion relation (Sparse coefficients constant) = do
  let finalRelation = case relation of
        Equal -> EqualRel
        LessThan -> OrderRel Lt
        LessThanOrEqual -> OrderRel Le

  let rationalVarCoefs = swap <$> Map.toList coefficients
  let (userVarCoefs, networkVarCoefs) = force $ partitionEithers (fmap splitRationalVar rationalVarCoefs)

  unless (null userVarCoefs) $
    compilerDeveloperError $
      "Found unsolved user variables" <+> pretty (fmap snd userVarCoefs)

  finalLHS <- case networkVarCoefs of
    (c : cs) -> return $ c :| cs
    [] -> compilerDeveloperError "Found trivial assertion"

  let finalRHS = -constant

  return $
    QueryAssertion
      { lhs = finalLHS,
        rel = finalRelation,
        rhs = finalRHS
      }

splitRationalVar ::
  (Coefficient, RationalVariable) ->
  Either (Coefficient, UserRationalVariable) (Coefficient, NetworkRationalVariable)
splitRationalVar (c, var) = case var of
  UserRationalVar v -> Left (c, v)
  NetworkRationalVar v -> Right (c, v)

--------------------------------------------------------------------------------
-- Step 3: calculate the actual set of network applications involved

data ReindexingState = ReindexingState
  { inputOutputIndices :: (Int, Int),
    oldVarSubstitution :: Map NetworkRationalVariable NetworkRationalVariable,
    metaNetworkEntries :: MetaNetwork
  }

calculateMetaNetwork ::
  forall m f.
  (MonadQueryStructure m, Traversable f) =>
  f QueryAssertion ->
  m (MetaNetwork, f QueryAssertion)
calculateMetaNetwork queries = do
  ctx <- get
  -- First calculate the set of network applications actually used in the query
  let referencedVars = foldMap queryAssertionVariables queries
  let networkApps = snd <$> LinkedHashMap.toList (networkApplications ctx)
  let usedNetworkApps = filter (isApplicationUsed referencedVars) networkApps
  -- Then perform the reindexing step to fix the indexing of the embedding variables.
  let initialState = ReindexingState (0, 0) mempty (MetaNetwork mempty mempty)
  finalState <- foldM reindex initialState usedNetworkApps
  reindexedQueries <- traverse (substAssertionVars (oldVarSubstitution finalState)) queries
  -- Return the final meta network and the reindexedQueries
  return (metaNetworkEntries finalState, reindexedQueries)
  where
    isApplicationUsed ::
      Set NetworkRationalVariable ->
      NetworkApplicationReplacement ->
      Bool
    isApplicationUsed referencedVars app = do
      let appVars = Set.fromList (inputRationalVars app <> outputRationalVars app)
      not $ Set.disjoint referencedVars appVars

    reindex :: ReindexingState -> NetworkApplicationReplacement -> m ReindexingState
    reindex ReindexingState {..} NetworkApplicationReplacement {..} = do
      let (inputIndex, outputIndex) = inputOutputIndices
      let MetaNetwork entries vars = metaNetworkEntries

      -- Calculate the reindexed variables
      let reindexedInputVars = fmap (reindexVar inputIndex) inputRationalVars
      let reindexedOutputVars = fmap (reindexVar outputIndex) outputRationalVars
      let reindexedVars = reindexedInputVars <> reindexedOutputVars

      -- Calculate substitution
      let newSubst = Map.fromList (zip inputRationalVars reindexedInputVars <> zip outputRationalVars reindexedOutputVars)

      -- Calculate new entry
      let entry = MetaNetworkEntry (fst networkApp) networkInfo

      return $
        ReindexingState
          { inputOutputIndices = (inputIndex + length inputRationalVars, outputIndex + length outputRationalVars),
            oldVarSubstitution = newSubst <> oldVarSubstitution,
            metaNetworkEntries = MetaNetwork (entries <> [entry]) (vars <> reindexedVars)
          }

    reindexVar :: Int -> NetworkRationalVariable -> NetworkRationalVariable
    reindexVar startIndex (ReducedVariable {originalVar = OriginalNetworkVariable {..}, ..}) =
      ReducedVariable
        { originalVar =
            OriginalNetworkVariable
              { startingIndex = startIndex,
                ..
              },
          ..
        }

    substAssertionVars :: Map NetworkRationalVariable NetworkRationalVariable -> QueryAssertion -> m QueryAssertion
    substAssertionVars subst QueryAssertion {..} = do
      newLHS <- traverse (\(c, v) -> (c,) <$> substVar v) lhs
      return $ QueryAssertion {lhs = newLHS, ..}
      where
        substVar :: NetworkRationalVariable -> m NetworkRationalVariable
        substVar var = case Map.lookup var subst of
          Nothing -> compilerDeveloperError "Malformed network variable subsitution"
          Just newVar -> return newVar

--------------------------------------------------------------------------------
-- Step 4: query assertions

-- | Checks for presence of under-constrained input variables.
checkIfNetworkInputsBounded ::
  (MonadCompile m, MonadReader PropertyMetaData m) =>
  QueryAddress ->
  MetaNetwork ->
  ConjunctAll QueryAssertion ->
  m ()
checkIfNetworkInputsBounded queryAddress metaNetwork constraints = do
  PropertyMetaData {..} <- ask
  let inputVariables = filter (\var -> inputOrOutput (originalVar var) == Input) (variables metaNetwork)
  finalStatuses <- variableConstraintStatus inputVariables constraints
  logDebug MaxDetail $ pretty constraints
  logDebug MaxDetail $ prettyMap finalStatuses

  -- If Marabou, then warn if all inputs are constant.
  -- See https://github.com/NeuralNetworkVerification/Marabou/issues/670
  let format = queryFormatID queryFormat
  when (format == MarabouQueries && all (== Constant) finalStatuses) $
    logWarning $
      AllConstantNetworkInputVars (queryFormatID queryFormat) queryAddress

  -- Check if all inputs are well-specified.
  let unboundedVariables = Map.toList $ Map.mapMaybe toUnderConstrainedStatus finalStatuses
  unless (null unboundedVariables) $
    logWarning $
      UnboundedNetworkInputVariables (queryFormatID queryFormat) queryAddress metaNetwork unboundedVariables

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
  [NetworkRationalVariable] ->
  ConjunctAll QueryAssertion ->
  m (Map NetworkRationalVariable VariableConstraintStatus)
variableConstraintStatus variables constraints = do
  let initialStatus = Map.fromList (fmap (,UnderConstrained Unconstrained) variables)
  return $ foldr updateStatuses initialStatus constraints
  where
    updateStatuses ::
      QueryAssertion ->
      Map NetworkRationalVariable VariableConstraintStatus ->
      Map NetworkRationalVariable VariableConstraintStatus
    updateStatuses assertion statuses = case lhs assertion of
      (c, v) :| [] | inputOrOutput (originalVar v) == Input -> do
        let status = case rel assertion of
              EqualRel -> Constant
              OrderRel op
                | (c >= 0) `xor` (op == Le || op == Lt) -> UnderConstrained BoundedBelow
                | otherwise -> UnderConstrained BoundedAbove
        Map.insertWith (<>) v status statuses
      _ -> statuses

--------------------------------------------------------------------------------
-- Step 5: prettyify assertions

prettifyQueryContents ::
  GenericBoundCtx NetworkRationalVariable ->
  ConjunctAll QueryAssertion ->
  QueryContents
prettifyQueryContents ctx conjuncts = do
  let optimisedConjuncts = fmap optimiseAssertionReadability conjuncts
  let sortedConjuncts = ConjunctAll $ NonEmpty.sortBy compareAssertion (unConjunctAll optimisedConjuncts)
  QueryContents ctx sortedConjuncts

-- | Applies various optimisations to an assertion to improve readability:
optimiseAssertionReadability :: QueryAssertion -> QueryAssertion
optimiseAssertionReadability (QueryAssertion lhs rel rhs) = do
  -- Sort the coefficients, first by input/output status and then by index
  let getKey (_, var) = (inputOrOutput (originalVar var), tensorIndices var)
  let sortedLHS = NonEmpty.sortWith getKey lhs

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

compareAssertion :: QueryAssertion -> QueryAssertion -> Ordering
compareAssertion e1 e2 =
  compareExpression (lhs e1) (lhs e2)
    `thenCmp` compare (rel e1) (rel e2)
    `thenCmp` compare (rhs e1) (rhs e2)

compareExpression ::
  NonEmpty (Coefficient, NetworkRationalVariable) ->
  NonEmpty (Coefficient, NetworkRationalVariable) ->
  Ordering
compareExpression expr1 expr2 =
  compare (length expr1 == 1) (length expr2 == 1) -- Put variable bounds first
    `thenCmp` compare (fmap snd expr1) (fmap snd expr2)
