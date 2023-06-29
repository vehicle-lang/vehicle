module Vehicle.Compile.Queries.VariableReconstruction where

import Data.Foldable (foldlM)
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
  (MonadLogger m) =>
  MetaNetwork ->
  VariableNormalisationSteps ->
  NetworkVariableAssignment ->
  m UserVariableAssignment
reconstructUserVars metaNetwork steps networkVariableAssignment =
  logCompilerPass MidDetail "recreation of user variables" $ do
    let assignment = createInitialAssignment metaNetwork networkVariableAssignment
    logDebug MaxDetail $ "Network variables:" <+> pretty (Map.keys assignment)
    finalAssignment <- foldlM applyReconstructionStep assignment (reverse steps)
    let assignmentPairs = traverse checkIsUserVariable $ Map.toList finalAssignment
    case assignmentPairs of
      Right pairs ->
        return $ UserVariableAssignment pairs
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
  (MonadLogger m) =>
  VariableAssignment MixedVariable ->
  VariableNormalisationStep ->
  m (VariableAssignment MixedVariable)
applyReconstructionStep assignment step =
  case step of
    Reduce var -> unreduceVariable var assignment
    Introduce var -> do
      logDebug MaxDetail $ "Deleting now unused variable" <+> quotePretty var
      return $ Map.delete var assignment
    EliminateViaGaussian var solution -> do
      logDebug MaxDetail $ "Reintroducing Gaussian solved variable" <+> quotePretty var
      let errorOrValue = reconstructGaussianVariableValue assignment solution
      case errorOrValue of
        Left missingVar -> developerError $ "Missing variable required in Gaussian elimination" <+> quotePretty missingVar
        Right value -> return $ Map.insert var value assignment
    EliminateViaFourierMotzkin var solution -> do
      logDebug MaxDetail $ "Reintroducing Fourier-Motzkin solved variable" <+> pretty var
      let errorOrValue = reconstructFourierMotzkinVariableValue assignment solution
      case errorOrValue of
        Left missingVar -> developerError $ "Missing variable required in Fourier-Motzkin elimination" <+> quotePretty missingVar
        Right value -> return $ Map.insert var value assignment

-- | Unreduces a previously reduced variable, removing the normalised
-- values from the assignment and adding the unreduced value back to the
-- assignment.
unreduceVariable ::
  (MonadLogger m) =>
  MixedVariable ->
  VariableAssignment MixedVariable ->
  m (VariableAssignment MixedVariable)
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
      logDebug MaxDetail $
        "Collapsing variables" <+> pretty reducedVariables <+> "to single variable" <+> pretty variable
      let unreducedValue = Vector.concat values
      return $ Map.insert variable unreducedValue assignment'

checkIsUserVariable :: (MixedVariable, Constant) -> Either NetworkVariable (UserVariable, Constant)
checkIsUserVariable (var, value) = case var of
  NetworkVar v -> Left v
  UserVar v -> Right (v, value)
