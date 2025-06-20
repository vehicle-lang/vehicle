module Vehicle.Backend.Queries.UserVariableElimination.VariableReconstruction where

import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Vehicle.Compile.FourierMotzkinElimination
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (prettyFriendly)
import Vehicle.Data.Code.LinearExpr (VariableLike (..), evaluateExpr)
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (RatTensor, Tensor (..), toList, traverseTensor, pattern ZeroDimTensor)
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
  logCompilerPass MidDetail "calculation of problem space witness" $ do
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

type MixedVariableAssignment = Map TensorVariable RatTensor

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

  let (newVar, valueOrErrorFn, derivedValuesFn) = case step of
        SolveEquality var childVars eq -> (toTensorVar var, evaluateExpr eq, constructElementsFromTensor childVars)
        SolveInequalities var solution -> (toTensorVar var, reconstructFourierMotzkinVariableValue solution, const [])
        ReconstructTensorVariable var elements -> (toTensorVar var, constructTensorVariableFromElements elements, const [])
  let value = handleMissingError ctx newVar (valueOrErrorFn assignment)

  logDebugM MidDetail $ do
    let varDoc = prettyFriendly (WithContext newVar ctx)
    return $ "Result:" <+> varDoc <+> "=" <+> pretty value

  let derivedValues = derivedValuesFn value
  let newValues = Map.fromList $ (newVar, value) : derivedValues

  return $ Map.union newValues assignment

-- | Unreduces a previously reduced variable, removing the normalised
-- values from the assignment and adding the unreduced value back to the
-- assignment.
constructTensorVariableFromElements ::
  Tensor TensorVariable ->
  MixedVariableAssignment ->
  Either TensorVariable RatTensor
constructTensorVariableFromElements elementVariables assignment =
  traverseTensor (lookupElementVariable assignment) (coerce elementVariables)
  where
    -- \| Lookups the values in the variable assignment and removes them from the
    -- assignment. Returns either the first missing variable or the list of values
    -- and the resulting assignment.
    lookupElementVariable ::
      (VariableLike variable) =>
      Map variable RatTensor ->
      variable ->
      Either variable Rational
    lookupElementVariable values v = case Map.lookup v values of
      Nothing -> Left v
      Just (ZeroDimTensor value) -> Right value
      Just _ -> developerError "Element variables should have an empty tensor shape"

constructElementsFromTensor ::
  [TensorVariable] ->
  RatTensor ->
  [(TensorVariable, RatTensor)]
constructElementsFromTensor elementVars value = do
  let elements = ZeroDimTensor <$> toList value
  zip elementVars elements

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
    isUserVar :: (TensorVariable, RatTensor) -> Maybe (Name, RatTensor)
    isUserVar (var, value) =
      if Set.member (coerce var) userVariables
        then do
          let name = lookupLvInBoundCtx (toLv var) vehicleVariables
          Just (name, value)
        else Nothing

--------------------------------------------------------------------------------
-- Utilities

handleMissingError ::
  CompleteNamedBoundCtx ->
  TensorVariable ->
  Either TensorVariable a ->
  a
handleMissingError ctx var errorOrResult = case errorOrResult of
  Right result -> result
  Left missingVar -> do
    developerError $
      "When reconstructing variable"
        <+> prettyFriendly (WithContext var ctx)
        <+> "in counter-example,"
        <+> "unable to find variable"
        <+> prettyFriendly (WithContext missingVar ctx)
