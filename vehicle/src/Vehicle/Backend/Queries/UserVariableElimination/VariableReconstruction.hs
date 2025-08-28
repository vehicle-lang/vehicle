module Vehicle.Backend.Queries.UserVariableElimination.VariableReconstruction
  ( reconstructUserVars,
  )
where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.FourierMotzkinElimination
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Assertion (Bounds)
import Vehicle.Data.Code.LinearExpr (LinearExpr, VariableLike (..), evaluateExpr)
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (RatTensor, at, shapeOf, stack, pattern ZeroDimTensor)
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Specification
import Vehicle.Verify.Verifier.Core

--------------------------------------------------------------------------------
-- Variable reconstruction

reconstructUserVars ::
  (MonadLogger m) =>
  VariableStore ->
  VariableCompilationTrace ->
  QueryVariableAssignment ->
  m UserVariableAssignment
reconstructUserVars variables (Reconstruction steps) networkVariableAssignment =
  logCompilerPass WitnessReconstruction $ do
    let queryVariableMap = getQueryVariableMap variables
    let vehicleVariableCtx = getVehicleVariableCtx variables
    let userVariables = getUserVariables variables
    let assignment = createInitialAssignment queryVariableMap networkVariableAssignment
    alteredAssignment <- foldlM (applyReconstructionStep vehicleVariableCtx) assignment steps
    finalAssignment <- createFinalAssignment vehicleVariableCtx userVariables alteredAssignment
    logDebug MidDetail $ "User variables:" <> lineIndent (pretty finalAssignment)
    return finalAssignment

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
            (_ : ds, Just childVars) -> do
              (elements, assignments) <- unzipF <$> traverse (go (depth - 1)) childVars
              let result = stack ds elements
              return (result, (sliceVar, result) :| concatMap NonEmpty.toList assignments)
            _ -> throwError (toSliceVar variable, MismatchedDimensions depthToReconstruct (length (shapeOf variable)))
      where
        sliceVar = toSliceVar var

reconstructTensorViaEquality ::
  (MonadReconstruct m) =>
  NestedSliceVariable ->
  LinearExpr SliceVariable RatTensor ->
  MixedVariableAssignment ->
  m (NonEmpty (SliceVariable, RatTensor))
reconstructTensorViaEquality variable equality assignment = do
  let errorOrValue = evaluateExpr equality assignment
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
  Bounds SliceVariable RatTensor ->
  MixedVariableAssignment ->
  m (NonEmpty (SliceVariable, RatTensor))
reconstructRationalViaFourierMotzkin var bounds assignment = do
  let result = reconstructFourierMotzkinVariableValue bounds assignment
  case result of
    Left missingVar -> throwError (var, MissingVariable missingVar)
    Right value -> return [(var, value)]

createFinalAssignment ::
  (MonadLogger m) =>
  CompleteNamedBoundCtx ->
  Set UserVariable ->
  MixedVariableAssignment ->
  m UserVariableAssignment
createFinalAssignment vehicleVariables userVariables assignment = do
  let userVariableValues = mapMaybe isUserVar $ Map.toList assignment
  return $ UserVariableAssignment userVariableValues
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
