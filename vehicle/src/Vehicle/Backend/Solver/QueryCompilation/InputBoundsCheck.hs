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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.These (These (..))
import Vehicle.Backend.Solver.UserVariableElimination.Core
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Compile.Resource (NetworkName)
import Vehicle.Data.Bound
import Vehicle.Data.Builtin.Standard.Core (Builtin)
import Vehicle.Data.Code.BooleanExpr
import Vehicle.Data.Code.Value (boundVariablesIn)
import Vehicle.Data.MaybeTrivial (MonadMaybeTrivial (..))
import Vehicle.Data.Tensor (HasShape (..), RatTensor, allTensor, anyTensor, zipWithTensor)
import Vehicle.Data.Tensor.Traversal (toPartialShape)
import Vehicle.Data.Variable.Bound.Context.Name (runNameContextT)
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Data.Variable.Bound.Tensor (findCorrespondingTensorVariable, findIndices, nestedCtxToNameCtx)
import Vehicle.Data.Variable.Free.Context (runFreshFreeContextT)
import Vehicle.Prelude.Warning (CompileWarning (..))
import Vehicle.Verify.QueryFormat.Core (QueryFormatID (..))
import Vehicle.Verify.QueryFormat.Interface (QueryFormat (..))

--------------------------------------------------------------------------------
-- Interface

-- | Checks for presence of under-constrained input variables.
findInputVariableBounds ::
  (MonadCompile m, MonadMaybeTrivial m) =>
  PropertyMetaData ->
  GlobalCtx ->
  NetworkApplications ->
  ConjunctAll LinearAssertion ->
  m (BoundedAssertions NetworkInputTensorVariable SliceVariable RatTensor)
findInputVariableBounds metaData ctx metaNetworkApps constraints = do
  logCompilerSection2 MaxDetail "network variable bounds checks" $
    runMonadBoundsT metaData ctx metaNetworkApps $ do
      -- Search through the list of constraints for bounds
      boundedAssertions <- findBoundsInConjuncts constraints

      logDebugM MaxDetail $ do
        let (assertionDoc, boundsDoc) = prettyBoundedAssertions boundedAssertions (completeNamedCtx ctx)
        return $ "Found bounds:" <> lineIndent boundsDoc <> line <> "remaining assertions:" <> lineIndent assertionDoc

      -- Check that all bounds present
      checkAllBoundsPresent boundedAssertions

--------------------------------------------------------------------------------
-- MonadSearch

type BoundsState = (PropertyMetaData, GlobalCtx, Map NetworkInputTensorVariable (NetworkName, NetworkApplicationInfo))

isNetworkTensorInputVar :: BoundsState -> SliceVariable -> Maybe NetworkInputTensorVariable
isNetworkTensorInputVar (_, _, networkNames) var = do
  let tentativeInputVar = coerce var
  if tentativeInputVar `Map.member` networkNames
    then Just tentativeInputVar
    else Nothing

type MonadBounds m =
  ( MonadCompile m,
    MonadReader BoundsState m
  )

runMonadBoundsT ::
  (MonadLogger m) =>
  PropertyMetaData ->
  GlobalCtx ->
  NetworkApplications ->
  ReaderT BoundsState m a ->
  m a
runMonadBoundsT propertyMetaData globalCtx networkApps action = do
  -- Make the mapping from input variables to names
  let mkInputVarEntry (name, app) = (inputVariable app, (name, app))
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
andPartiallyBoundedAssertions a@(Partial bounds1 assertions1) b@(Partial bounds2 assertions2) = do
  newVariableBounds <- unionWithM andTensorBounds bounds1 bounds2
  let newAssertions = assertions1 <> assertions2
  let c = Partial newVariableBounds newAssertions
  (_, ctx, _) <- ask
  logDebug MaxDetail "and"
  incrCallDepth
  logDebug MaxDetail $ snd $ prettyBoundedAssertions a (completeNamedCtx ctx)
  logDebug MaxDetail $ snd $ prettyBoundedAssertions b (completeNamedCtx ctx)
  logDebug MaxDetail $ snd $ prettyBoundedAssertions c (completeNamedCtx ctx)
  decrCallDepth
  return c

prettyBoundedAssertions :: PartiallyBoundedAssertions -> CompleteNamedBoundCtx -> (Doc a, Doc a)
prettyBoundedAssertions (Partial bounds assertions) ctx = do
  let assertionsDoc = prettyFriendly (WithContext assertions ctx)
  let boundsDoc = vsep $ fmap boundToDoc (Map.toList bounds)
  (assertionsDoc, boundsDoc)
  where
    boundToDoc :: (NetworkInputTensorVariable, TensorBounds RatTensor) -> Doc a
    boundToDoc (var, partialBounds) = do
      let varDoc = prettyFriendly (WithContext var ctx)
      let boundDoc = prettyFriendly (WithContext (BoundedValue var partialBounds) ctx)
      varDoc <> ":" <> lineIndent boundDoc

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
  state@(_, _, _) <- ask
  return $ case tryToConvertToTensorBounds (lookupCorrespondingInputVar state) assertion of
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
  BoundsState ->
  SliceVariable ->
  Maybe VariableInfo
lookupCorrespondingInputVar state@(_, ctx, _) var = do
  let nestedSliceVar = findCorrespondingTensorVariable (globalBoundVarCtx ctx) var
  let maybeInputVar = isNetworkTensorInputVar state (toSliceVar nestedSliceVar)
  case maybeInputVar of
    Nothing -> Nothing
    Just inputVar -> do
      let indices = findIndices nestedSliceVar var
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
  m (BoundedAssertions NetworkInputTensorVariable SliceVariable RatTensor)
checkAllBoundsPresent (Partial allPartialbounds assertions) = do
  (PropertyMetaData {..}, ctx@GlobalCtx {..}, inputVariableMapping) <- ask

  let nameCtx = Just <$> nestedCtxToNameCtx globalBoundVarCtx
  errorsAndFinalBounds <- forM (Map.toList inputVariableMapping) $ \(var, (networkName, appInfo)) -> do
    let errorCase indices = Left (networkName, inputValue appInfo, findUnboundedVariables ctx appInfo, indices)
    case Map.lookup var allPartialbounds of
      Nothing -> return $ errorCase $ These [[]] [[]]
      Just partialBounds -> do
        missingIndicesOrFlattenedBounds <-
          runFreshFreeContextT (Proxy @Builtin) $
            runNameContextT nameCtx $
              flattenTensorBounds partialBounds

        case missingIndicesOrFlattenedBounds of
          Right bounds -> return $ Right (var, bounds)
          Left missingIndices -> do
            return $ errorCase missingIndices

  let (missingBounds, completeBounds) = partitionEithers errorsAndFinalBounds
  case missingBounds of
    [] -> return ()
    i : is -> throwError $ UnboundedNetworkInputVariables propertyProvenance (completeNamedCtx ctx) (i :| is)

  -- If Marabou, then warn if all inputs are constant.
  -- See https://github.com/NeuralNetworkVerification/Marabou/issues/670
  let formatID = queryFormatID queryFormat
  when (queryFormatID queryFormat == MarabouQueries && all isEquality completeBounds) $
    logWarning $
      AllConstantNetworkInputVars formatID propertyAddress

  let boundsUnsatisfiable = any isUnsatisfiable completeBounds
  if boundsUnsatisfiable
    then trivial False
    else case assertions of
      [] -> trivial True
      a : as ->
        return $
          BoundedAssertions
            { variableBounds = Map.fromList completeBounds,
              assertions = ConjunctAll (a :| as)
            }

isUnsatisfiable :: (NetworkInputTensorVariable, (RatTensor, RatTensor)) -> Bool
isUnsatisfiable (_name, (lower, upper)) = anyTensor id $ zipWithTensor (>) lower upper

isEquality :: (NetworkInputTensorVariable, (RatTensor, RatTensor)) -> Bool
isEquality (_name, (lower, upper)) = allTensor id $ zipWithTensor (==) lower upper

findUnboundedVariables :: GlobalCtx -> NetworkApplicationInfo -> [Name]
findUnboundedVariables globalCtx appInfo = do
  -- TODO we actually need to do this recursively on any network variables that
  -- live in this set.
  let boundVars = Set.toList $ boundVariablesIn $ inputValue appInfo
  fmap (\lv -> lookupLvInBoundCtx lv (completeNamedCtx globalCtx)) boundVars
