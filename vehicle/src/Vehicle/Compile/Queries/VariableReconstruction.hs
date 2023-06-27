module Vehicle.Compile.Queries.VariableReconstruction where

import Data.Map qualified as Map
import Data.Vector.Unboxed qualified as Vector
import Vehicle.Compile.Prelude
import Vehicle.Compile.Queries.FourierMotzkinElimination
import Vehicle.Compile.Queries.GaussianElimination
import Vehicle.Compile.Queries.Variable
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Variable reconstruction

reconstructUserVars ::
  MetaNetwork ->
  VariableNormalisationSteps ->
  NetworkVariableAssignment ->
  UserVariableAssignment
reconstructUserVars metaNetwork steps networkVariableAssignment = do
  let assignment = createInitialAssignment metaNetwork networkVariableAssignment
  let finalAssignment = foldr applyReconstructionStep assignment (reverse steps)
  let assignmentPairs = traverse checkIsUserVariable $ Map.toList finalAssignment
  case assignmentPairs of
    Right pairs -> UserVariableAssignment pairs
    Left networkVar ->
      developerError $ "Did not successfully eliminate network variable" <+> pretty networkVar

createInitialAssignment ::
  MetaNetwork ->
  NetworkVariableAssignment ->
  VariableAssignment MixedVariable
createInitialAssignment metaNetwork (NetworkVariableAssignment values) = do
  let networkVariables = metaNetworkVariables True metaNetwork
  let mkEntry var value = (NetworkVar var, Vector.singleton value)
  let variableValuePairs = zipWith mkEntry networkVariables (Vector.toList values)
  Map.fromList variableValuePairs

applyReconstructionStep ::
  VariableNormalisationStep ->
  VariableAssignment MixedVariable ->
  VariableAssignment MixedVariable
applyReconstructionStep step assignment = case step of
  Reduce var ->
    unreduceVariable var assignment
  Introduce var ->
    Map.delete var assignment
  EliminateViaGaussian var solution -> do
    let errorOrValue = reconstructGaussianVariableValue assignment solution
    case errorOrValue of
      Left missingVar -> developerError $ "Missing variable required in Gaussian elimination" <+> pretty missingVar
      Right value -> Map.insert var value assignment
  EliminateViaFourierMotzkin var solution -> do
    let errorOrValue = reconstructFourierMotzkinVariableValue assignment solution
    case errorOrValue of
      Left missingVar -> developerError $ "Missing variable required in Fourier-Motzkin elimination" <+> pretty missingVar
      Right value -> Map.insert var value assignment

-- | Unreduces a previously reduced variable, removing the normalised
-- values from the assignment and adding the unreduced value back to the
-- assignment.
unreduceVariable ::
  MixedVariable ->
  VariableAssignment MixedVariable ->
  VariableAssignment MixedVariable
unreduceVariable variable assignment = do
  let reducedVariables = fst $ reduceVariable 0 variable
  let variableResults = lookupAndRemoveAll assignment reducedVariables
  case variableResults of
    Left missingVar ->
      developerError $
        "When reconstructing variable"
          <+> pretty variable
          <+> "in counter-example,"
          <+> "unable to find variable"
          <+> pretty missingVar
    Right (values, assignment') -> do
      let unreducedValue = Vector.concat values
      Map.insert variable unreducedValue assignment'

checkIsUserVariable :: (MixedVariable, Constant) -> Either NetworkVariable (UserVariable, Constant)
checkIsUserVariable (var, value) = case var of
  NetworkVar v -> Left v
  UserVar v -> Right (v, value)
