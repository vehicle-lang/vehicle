module Vehicle.Backend.Solver.UserVariableElimination.VariableReconstruction
  ( reconstructUserVars,
  )
where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Identity (Identity (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.List (delete)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.Constants.Rational
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion (InequalityRelation (..))
import Vehicle.Data.Bound
import Vehicle.Data.Code.LinearExpr (LinearExpr, evaluateExpr)
import Vehicle.Data.Tensor (RatTensor, at, mapTensor, shapeOf, stack, unstack, zipWithTensor, pattern ZeroDimTensor)
import Vehicle.Data.Variable.Bound.Context.Name.Core
import Vehicle.Data.Variable.Bound.Level
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Solver
import Vehicle.Verify.Specification

--------------------------------------------------------------------------------
-- Variable reconstruction

reconstructUserVars ::
  (MonadLogger m) =>
  VariableStore ->
  VariableCompilationTrace ->
  QueryVariableAssignment ->
  m UserVariableAssignment
reconstructUserVars variables (Reconstruction steps) networkVariableAssignment = do
  let queryVariableMap = getQueryVariableMap variables
  let vehicleVariableCtx = getVehicleVariableCtx variables
  let userVariables = getUserVariables variables
  let assignment = createInitialAssignment queryVariableMap networkVariableAssignment
  alteredAssignment <- foldlM (applyReconstructionStep vehicleVariableCtx) assignment steps
  finalAssignment <- createFinalAssignment vehicleVariableCtx userVariables alteredAssignment
  recordSubstAssignment <- reconstructRecords finalAssignment steps
  logDebug MidDetail $ "User variables:" <> lineIndent (pretty recordSubstAssignment)
  return recordSubstAssignment

reconstructRecords ::
  (MonadLogger m) =>
  UserVariableAssignment ->
  [CompilationStep] ->
  m UserVariableAssignment
reconstructRecords existingAssignment steps = do
  foldlM checkStep existingAssignment steps
  where
    checkStep (UserVariableAssignment assignments) step = do
      case step of
        ConvertQuantifiedTensorLike tensorName recordName fieldNames -> do
          tensorValues <- case Map.lookup tensorName (Map.fromList assignments) of
            Just (TensorValue v) -> pure v
            _ -> developerError "No assignment found"
          tensorIndices <- case NonEmpty.nonEmpty (unstack tensorValues) of
            Just xs -> pure xs
            _ -> developerError "Values must be present for tensor assignment"

          let fields = NonEmpty.zip fieldNames tensorIndices
          let assignment = (recordName, RecordValue fields)
          let newMap = delete (tensorName, TensorValue tensorValues) assignments ++ [assignment]

          return $ UserVariableAssignment newMap
        _ -> return $ UserVariableAssignment assignments

--------------------------------------------------------------------------------
-- Mixed variable assignments

type MixedVariableAssignment = Map SliceVariable RatTensor

prettyAssignment :: CompleteNamedBoundCtx -> MixedVariableAssignment -> Doc a
prettyAssignment ctx assignment = do
  let prettyVar v = prettyFriendly (WithContext v ctx)
  prettyMapEntries (bimap prettyVar pretty <$> Map.toList assignment)

createInitialAssignment ::
  Map QueryVariable NetworkIOElementVariable ->
  QueryVariableAssignment ->
  MixedVariableAssignment
createInitialAssignment queryVariableMap (QueryVariableAssignment valuesByQueryVar) = do
  let missingVariable var = developerError ("Solver returned additional unknown variable" <+> pretty var)
  let mapQueryVariable var = coerce $ fromMaybe (missingVariable var) (Map.lookup var queryVariableMap)
  let valuesByNetworkVar = ZeroDimTensor <$> Map.mapKeys mapQueryVariable valuesByQueryVar
  valuesByNetworkVar

applyReconstructionStep ::
  (MonadLogger m) =>
  CompleteNamedBoundCtx ->
  MixedVariableAssignment ->
  CompilationStep ->
  m MixedVariableAssignment
applyReconstructionStep ctx assignment step = do
  logDebug MidDetail $ "Variable assignment:" <> lineIndent (prettyAssignment ctx assignment)
  logDebug MidDetail $ prettyFriendly (WithContext step ctx)

  let errorOrValueFn = case step of
        SolveEquality nestedVar eq -> reconstructTensorViaEquality nestedVar eq
        SolveInequalities var solution -> reconstructRationalViaFourierMotzkin var solution
        ReconstructTensorVariable var depth -> reconstructTensorFromConstituents ctx var depth
        -- do nothing if we have convertTensorLike
        -- TODO: this is not nice at all, maybe we need to store the compilationStep
        -- differently or convert a different way?
        ConvertQuantifiedTensorLike {} -> \varAssignment ->
          case NonEmpty.nonEmpty (Map.toList varAssignment) of
            Just a -> pure a
            Nothing -> developerError "Variable assignment list should not be empty"
  newValues <- handleMissingError ctx (errorOrValueFn assignment)

  logDebugM MidDetail $ do
    let (newVar, newHead) :| remainder = newValues
    let varDoc = prettyFriendly (WithContext newVar ctx)
    return $ "Result:" <+> varDoc <+> "=" <+> pretty newHead <+> parens (pretty (length remainder) <+> "others")

  return $ Map.union (Map.fromList $ NonEmpty.toList newValues) assignment

data ReconstructionError
  = MissingVariable SliceVariable
  | MismatchedDimensions Int Int

type MonadReconstruct m =
  ( MonadLogger m,
    MonadError (SliceVariable, ReconstructionError) m
  )

-- | Unreduces a previously reduced variable, removing the normalised
-- values from the assignment and adding the unreduced value back to the
-- assignment.
reconstructTensorFromConstituents ::
  forall m.
  (MonadReconstruct m) =>
  CompleteNamedBoundCtx ->
  NestedSliceVariable ->
  ReconstructionDepth ->
  MixedVariableAssignment ->
  m (NonEmpty (SliceVariable, RatTensor))
reconstructTensorFromConstituents _ctx variable reconstructionDepth assignment =
  snd <$> go depthToReconstruct variable
  where
    depthToReconstruct :: Int
    depthToReconstruct = case reconstructionDepth of
      OneDimension -> 1
      AllDimensions -> length (shapeOf variable)

    go :: Int -> NestedSliceVariable -> m (RatTensor, NonEmpty (SliceVariable, RatTensor))
    go depth var
      | depth == 0 =
          case Map.lookup sliceVar assignment of
            Nothing -> throwError (toSliceVar variable, MissingVariable sliceVar)
            Just result -> return (result, [(sliceVar, result)])
      | otherwise =
          case (shapeOf var, childVariablesOf var) of
            (_ : dims, Just childVars) -> do
              (elements, assignments) <- unzipF <$> traverse (go (depth - 1)) childVars
              let result = stack dims elements
              return (result, (sliceVar, result) :| concatMap NonEmpty.toList assignments)
            _ -> throwError (toSliceVar variable, MismatchedDimensions depthToReconstruct (length (shapeOf variable)))
      where
        sliceVar = toSliceVar var

reconstructTensorViaEquality ::
  (MonadReconstruct m) =>
  NestedSliceVariable ->
  LinearExpression ->
  MixedVariableAssignment ->
  m (NonEmpty (SliceVariable, RatTensor))
reconstructTensorViaEquality variable equality assignment = do
  errorOrValue <- evaluateExpr assignment equality
  case errorOrValue of
    Left missingVar -> throwError (toSliceVar variable, MissingVariable missingVar)
    Right value -> return $ go value variable
  where
    go :: RatTensor -> NestedSliceVariable -> NonEmpty (SliceVariable, RatTensor)
    go tensor var = do
      let tensorVar = toSliceVar var
      let childValues = case childVariablesOf var of
            Nothing -> []
            Just childVars -> do
              let goChild (childVar, index) = go (tensor `at` index) childVar
              concatMap (NonEmpty.toList . goChild) (zip childVars [0 ..])
      (tensorVar, tensor) :| childValues

reconstructRationalViaFourierMotzkin ::
  (MonadReconstruct m) =>
  SliceVariable ->
  LinearBounds ->
  MixedVariableAssignment ->
  m (NonEmpty (SliceVariable, RatTensor))
reconstructRationalViaFourierMotzkin var bounds assignment = do
  let result = reconstructFourierMotzkinVariableValue bounds assignment
  case result of
    Left missingVar -> throwError (var, MissingVariable missingVar)
    Right value -> return [(var, value)]

-- | Tries to reconstruct the value of the variable that is
-- consistent with the current assignment of variables. Returns either a
-- required variable that is missing from the assignment or the reconstructed
-- value.
reconstructFourierMotzkinVariableValue ::
  forall variable.
  (VariableLike variable) =>
  SliceBounds (LinearExpr variable RatTensor) ->
  Map variable RatTensor ->
  Either variable RatTensor
reconstructFourierMotzkinVariableValue solution assignment = do
  lowerBoundValues <- traverse (traverse (runIdentity . evaluateExpr assignment)) (lowerBounds solution)
  upperBoundValues <- traverse (traverse (runIdentity . evaluateExpr assignment)) (upperBounds solution)

  maybeLowerBound <- andBoundList lowerBoundValues
  maybeUpperBound <- andBoundList upperBoundValues

  return $ case (maybeLowerBound, maybeUpperBound) of
    (Nothing, Nothing) -> ZeroDimTensor 0
    (Just (LowerBound _ value), Nothing) -> mapTensor (+ 1) value
    (Nothing, Just (UpperBound _ value)) -> mapTensor (\x -> x - 1) value
    (Just (LowerBound rel1 value1), Just (UpperBound rel2 value2))
      -- UNSOUND over FP?
      | value1 < value2 || value1 == value2 && rel1 == NonStrict && rel2 == NonStrict ->
          zipWithTensor (\u v -> 0.5 * (u + v)) value1 value2
      | otherwise -> do
          -- Only 99% sure about this. Can't find a good reference to the reconstruction phase of the
          -- algorithm. Closest to referencing this impossibility is:
          -- https://people.math.carleton.ca/~kcheung/math/notes/MATH5801/02/2_1_fourier_motzkin.html
          developerError $
            "Fourier-Motzkin reconstruction failed with range" <+> pretty value1 <+> pretty rel1 <+> "<var>" <+> pretty rel2 <+> pretty value2
              <> ". This isn't supposed to be possible..."

createFinalAssignment ::
  (MonadLogger m) =>
  CompleteNamedBoundCtx ->
  Set UserSliceVariable ->
  MixedVariableAssignment ->
  m UserVariableAssignment
createFinalAssignment vehicleVariables userVariables assignment = do
  let userVariableValues = mapMaybe isUserVar $ Map.toList assignment
  return $ UserVariableAssignment (map (second TensorValue) userVariableValues)
  where
    isUserVar :: (SliceVariable, RatTensor) -> Maybe (Name, RatTensor)
    isUserVar (var, value) =
      if Set.member (coerce var) userVariables
        then do
          let name = lookupLvInBoundCtx (toLv var) vehicleVariables
          Just (name, value)
        else Nothing

--------------------------------------------------------------------------------
-- Utilities

handleMissingError ::
  (MonadLogger m) =>
  CompleteNamedBoundCtx ->
  ExceptT (SliceVariable, ReconstructionError) m a ->
  m a
handleMissingError ctx resultFn = do
  errorOrResult <- runExceptT resultFn
  case errorOrResult of
    Right result -> return result
    Left (targetVar, err) -> do
      developerError $
        "When reconstructing variable"
          <+> prettyFriendly (WithContext targetVar ctx)
          <+> "in counter-example,"
          <+> case err of
            MissingVariable missingVar -> "unable to find variable" <+> prettyFriendly (WithContext missingVar ctx)
            MismatchedDimensions expectedDepth actualDepth -> "expected tensor with at least" <+> pretty expectedDepth <+> "dimensions but found tensor with" <+> pretty actualDepth <+> "dimensions"
