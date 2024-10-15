module Vehicle.Backend.Queries.UserVariableElimination.VariableReconstruction where

import Control.Monad (foldM)
import Data.Foldable (foldlM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as HashMap
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vector
import Vehicle.Backend.Queries.UserVariableElimination.Core
import Vehicle.Compile.FourierMotzkinElimination
import Vehicle.Compile.Prelude
import Vehicle.Data.Code.LinearExpr (evaluateExpr)
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (RationalTensor, Tensor (..))
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Verifier.Core

--------------------------------------------------------------------------------
-- Variable reconstruction

reconstructUserVars ::
  (MonadLogger m) =>
  QueryVariableMapping ->
  UserVariableReconstruction ->
  QueryVariableAssignment ->
  m UserVariableAssignment
reconstructUserVars queryVariableMapping steps networkVariableAssignment =
  logCompilerPass MidDetail "calculation of problem space witness" $ do
    logDebug MidDetail $ pretty steps
    let assignment = createInitialAssignment queryVariableMapping networkVariableAssignment
    alteredAssignment <- foldlM applyReconstructionStep assignment steps
    let finalAssignment = createFinalAssignment alteredAssignment
    logDebug MidDetail $ "User variables:" <+> pretty finalAssignment
    return finalAssignment

--------------------------------------------------------------------------------
-- Mixed variable assignments

data MixedVariableAssignment = VariableAssignment
  { tensorVariables :: Map TensorVariable RationalTensor,
    rationalVariables :: Map RationalVariable Rational
  }

instance Pretty MixedVariableAssignment where
  pretty VariableAssignment {..} =
    "Tensor variables:" <+> prettyMap tensorVariables
      <> line
      <> "Rational variables:" <+> prettyMap rationalVariables
      <> line

-- | Lookup the value of the variable in an assignment.
lookupRationalVariable ::
  MixedVariableAssignment ->
  RationalVariable ->
  Maybe Rational
lookupRationalVariable VariableAssignment {..} var = Map.lookup var rationalVariables

-- | Lookups the values in the variable assignment and removes them from the
-- assignment. Returns either the first missing variable or the list of values
-- and the resulting assignment.
lookupRationalVariables ::
  MixedVariableAssignment ->
  [RationalVariable] ->
  Either RationalVariable [Rational]
lookupRationalVariables assignment = foldM op []
  where
    op values var = case lookupRationalVariable assignment var of
      Nothing -> Left var
      Just value -> Right (value : values)

createInitialAssignment ::
  [(QueryVariable, NetworkRationalVariable)] ->
  QueryVariableAssignment ->
  MixedVariableAssignment
createInitialAssignment queryVariableMapping (QueryVariableAssignment valuesByQueryVar) = do
  let queryVariableMap = Map.fromList queryVariableMapping
  let mapQueryVariable var = fromMaybe (developerError ("Missing query variable" <+> pretty var)) (Map.lookup var queryVariableMap)
  let valuesByNetworkVar = Map.mapKeys mapQueryVariable valuesByQueryVar
  VariableAssignment
    { rationalVariables = HashMap.mapKeys NetworkRationalVar valuesByNetworkVar,
      tensorVariables = mempty
    }

applyReconstructionStep ::
  (MonadLogger m) =>
  MixedVariableAssignment ->
  UserVariableReconstructionStep ->
  m MixedVariableAssignment
applyReconstructionStep assignment@VariableAssignment {..} step = do
  logDebug MidDetail $ "Variable assignment:" <> line <> indent 2 (pretty assignment)
  case step of
    ReconstructTensor var individualVars ->
      unreduceVariable var individualVars assignment
    SolveRationalEquality var eq -> do
      logCompilerSection MidDetail ("Reintroducing Gaussian-eliminated variable" <+> quotePretty var) $ do
        logDebug MidDetail $ "Using" <+> pretty step
        let errorOrValue = evaluateExpr eq rationalVariables
        case errorOrValue of
          Left missingVar -> developerError $ "Missing variable" <+> quotePretty missingVar <+> "required in Gaussian elimination of" <+> quotePretty var
          Right value -> do
            logDebug MidDetail $ "Result:" <+> pretty var <+> "=" <+> pretty value
            return $
              VariableAssignment
                { rationalVariables = Map.insert (UserRationalVar var) value rationalVariables,
                  ..
                }
    SolveTensorEquality var eq -> do
      logCompilerSection MidDetail ("Reintroducing Gaussian-eliminated variable" <+> quotePretty var) $ do
        logDebug MidDetail $ "Using" <+> pretty step
        let errorOrValue = evaluateExpr eq tensorVariables
        case errorOrValue of
          Left missingVar -> developerError $ "Missing variable" <+> quotePretty missingVar <+> "required in Gaussian elimination of" <+> quotePretty var
          Right value -> do
            logDebug MidDetail $ "Result:" <+> pretty var <+> "=" <+> pretty value
            return $
              VariableAssignment
                { tensorVariables = Map.insert var value tensorVariables,
                  ..
                }
    SolveRationalInequalities var solution -> do
      logCompilerSection MidDetail ("Reintroducing Fourier-Motzkin-eliminated variable" <+> quotePretty var) $ do
        let errorOrValue = reconstructFourierMotzkinVariableValue rationalVariables solution
        case errorOrValue of
          Left missingVar -> developerError $ "Missing variable" <+> quotePretty missingVar <+> "required in Fourier-Motzkin elimination of" <+> quotePretty var
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
  let variableResults = lookupRationalVariables assignment reducedVariables
  case variableResults of
    Left missingVar ->
      developerError $
        "When reconstructing variable"
          <+> pretty variable
          <+> "in counter-example,"
          <+> "unable to find variable"
          <+> pretty missingVar
    Right values -> do
      logDebug MaxDetail $
        "Collapsing variables" <+> pretty reducedVariables <+> "to single variable" <+> pretty variable
      let unreducedValue = Tensor (tensorVarDimensions variable) (Vector.fromList values)
      return $
        assignment
          { tensorVariables = Map.insert variable unreducedValue tensorVariables
          }

createFinalAssignment ::
  MixedVariableAssignment ->
  UserVariableAssignment
createFinalAssignment (VariableAssignment {..}) = do
  UserVariableAssignment $ Map.toList tensorVariables
