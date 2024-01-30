module Vehicle.Backend.Queries.UserVariableElimination.VariableReconstruction where

import Control.Monad (foldM)
import Data.Foldable (foldlM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as HashMap
import Data.Maybe (mapMaybe)
import Data.Vector qualified as Vector
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Backend.Queries.UserVariableElimination.FourierMotzkinElimination
import Vehicle.Compile.Prelude
import Vehicle.Data.LinearExpr (RationalTensor (..), evaluateExpr)
import Vehicle.Verify.Variable

--------------------------------------------------------------------------------
-- Variable reconstruction

reconstructUserVars ::
  (MonadLogger m) =>
  UserVariableReconstruction ->
  NetworkVariableAssignment ->
  m UserVariableAssignment
reconstructUserVars steps networkVariableAssignment =
  logCompilerPass MidDetail "recreation of user variables" $ do
    let assignment = createInitialAssignment networkVariableAssignment
    logDebug MaxDetail $ "Network variables:" <+> pretty networkVariableAssignment
    alteredAssignment <- foldlM applyReconstructionStep assignment (reverse steps)
    let finalAssignment = createFinalAssignment alteredAssignment
    return finalAssignment

--------------------------------------------------------------------------------
-- Mixed variable assignments

data MixedVariableAssignment = VariableAssignment
  { tensorVariables :: Map TensorVariable RationalTensor,
    rationalVariables :: Map RationalVariable Rational
  }

-- | Lookup the value of the variable in an assignment and remove it from the
-- assignment.
lookupAndRemoveRationalVariable ::
  MixedVariableAssignment ->
  RationalVariable ->
  Maybe (Rational, MixedVariableAssignment)
lookupAndRemoveRationalVariable VariableAssignment {..} var = do
  let (maybeValue, newRationalVariables) = Map.updateLookupWithKey (\_ _ -> Nothing) var rationalVariables
  case maybeValue of
    Nothing -> Nothing
    Just value -> Just (value, VariableAssignment {rationalVariables = newRationalVariables, ..})

-- | Lookups the values in the variable assignment and removes them from the
-- assignment. Returns either the first missing variable or the list of values
-- and the resulting assignment.
lookupAndRemoveAll ::
  MixedVariableAssignment ->
  [RationalVariable] ->
  Either RationalVariable ([Rational], MixedVariableAssignment)
lookupAndRemoveAll assignment = foldM op ([], assignment)
  where
    op (values, ass) var = case lookupAndRemoveRationalVariable ass var of
      Nothing -> Left var
      Just (value, ass') -> Right (value : values, ass')

createInitialAssignment ::
  NetworkVariableAssignment ->
  MixedVariableAssignment
createInitialAssignment (NetworkVariableAssignment values) = do
  VariableAssignment
    { rationalVariables = HashMap.mapKeys NetworkRationalVar values,
      tensorVariables = mempty
    }

applyReconstructionStep ::
  (MonadLogger m) =>
  MixedVariableAssignment ->
  UserVariableReconstructionStep ->
  m MixedVariableAssignment
applyReconstructionStep assignment@VariableAssignment {..} step =
  case step of
    ReconstructTensor var individualVars ->
      unreduceVariable (UserTensorVar var) (fmap UserRationalVar individualVars) assignment
    SolveRationalEquality var eq -> do
      logDebug MaxDetail $ "Reintroducing Gaussian solved variable" <+> quotePretty var
      let errorOrValue = evaluateExpr (rationalEqExpr eq) rationalVariables
      case errorOrValue of
        Left missingVar -> developerError $ "Missing variable required in Gaussian elimination" <+> quotePretty missingVar
        Right value ->
          return $
            VariableAssignment
              { rationalVariables = Map.insert (UserRationalVar var) value rationalVariables,
                ..
              }
    SolveTensorEquality var eq -> do
      logDebug MaxDetail $ "Reintroducing Gaussian solved variable" <+> quotePretty var
      let errorOrValue = evaluateExpr (tensorEqExpr eq) tensorVariables
      case errorOrValue of
        Left missingVar -> developerError $ "Missing variable required in Gaussian elimination" <+> quotePretty missingVar
        Right value ->
          return $
            VariableAssignment
              { tensorVariables = Map.insert (UserTensorVar var) value tensorVariables,
                ..
              }
    SolveRationalInequalities var solution -> do
      logDebug MaxDetail $ "Reintroducing Fourier-Motzkin solved variable" <+> pretty var
      let errorOrValue = reconstructFourierMotzkinVariableValue rationalVariables solution
      case errorOrValue of
        Left missingVar -> developerError $ "Missing variable required in Fourier-Motzkin elimination" <+> quotePretty missingVar
        Right value ->
          return $
            VariableAssignment
              { rationalVariables = Map.insert (UserRationalVar var) value rationalVariables,
                ..
              }

-- | Unreduces a previously reduced variable, removing the normalised
-- values from the assignment and adding the unreduced value back to the
-- assignment.
unreduceVariable ::
  (MonadLogger m) =>
  TensorVariable ->
  [RationalVariable] ->
  MixedVariableAssignment ->
  m MixedVariableAssignment
unreduceVariable variable reducedVariables assignment@VariableAssignment {..} = do
  let variableResults = lookupAndRemoveAll assignment reducedVariables
  case variableResults of
    Left missingVar ->
      developerError $
        "When reconstructing variable"
          <+> pretty variable
          <+> "in counter-example,"
          <+> "unable to find variable"
          <+> pretty missingVar
    Right (values, newAssignment) -> do
      logDebug MaxDetail $
        "Collapsing variables" <+> pretty reducedVariables <+> "to single variable" <+> pretty variable
      let unreducedValue = RationalTensor (tensorVariableDims variable) (Vector.fromList values)
      return $
        newAssignment
          { tensorVariables = Map.insert variable unreducedValue tensorVariables
          }

createFinalAssignment ::
  MixedVariableAssignment ->
  UserVariableAssignment
createFinalAssignment (VariableAssignment {..}) = do
  UserVariableAssignment $ mapMaybe getUserOriginalVariable (Map.toList tensorVariables)
  where
    getUserOriginalVariable :: (TensorVariable, RationalTensor) -> Maybe (OriginalUserVariable, RationalTensor)
    getUserOriginalVariable (var, value) = case var of
      NetworkTensorVar {} -> Nothing
      UserTensorVar v -> Just (v, value)
