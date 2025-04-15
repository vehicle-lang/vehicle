module Vehicle.Backend.Queries.UserVariableElimination.VariableReconstruction where

import Data.Foldable (foldlM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Vehicle.Compile.FourierMotzkinElimination
import Vehicle.Compile.Prelude
import Vehicle.Data.Assertion
import Vehicle.Data.Code.LinearExpr (LinearExpr, VariableLike (..), evaluateExpr)
import Vehicle.Data.QuantifiedVariable
import Vehicle.Data.Tensor (RatTensor, Tensor (..), pattern ZeroDimTensor)
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
    logDebug MidDetail $ pretty steps
    let assignment = createInitialAssignment queryVariableMap networkVariableAssignment
    alteredAssignment <- foldlM applyReconstructionStep assignment steps
    finalAssignment <- createFinalAssignment vehicleVariableCtx alteredAssignment
    logDebug MidDetail $ "User variables:" <+> pretty finalAssignment
    return finalAssignment

--------------------------------------------------------------------------------
-- Mixed variable assignments

data MixedVariableAssignment = VariableAssignment
  { userVariableValues :: Map UserTensorVariable RatTensor,
    networkVariableValues :: Map NetworkTensorVariable RatTensor
  }

instance Pretty MixedVariableAssignment where
  pretty VariableAssignment {..} =
    "User variable values:" <+> prettyMap userVariableValues
      <> line
      <> "Network variable values:" <+> pretty networkVariableValues

createInitialAssignment ::
  Map QueryVariable NetworkElementVariable ->
  QueryVariableAssignment ->
  MixedVariableAssignment
createInitialAssignment queryVariableMap (QueryVariableAssignment valuesByQueryVar) = do
  let missingVariable var = developerError ("Missing query variable" <+> pretty var)
  let mapQueryVariable var = _ $ fromMaybe (missingVariable var) (Map.lookup var queryVariableMap)
  let valuesByNetworkVar = ZeroDimTensor <$> Map.mapKeys mapQueryVariable valuesByQueryVar
  VariableAssignment
    { networkVariableValues = valuesByNetworkVar,
      userVariableValues = mempty
    }

applyReconstructionStep ::
  (MonadLogger m) =>
  NamedBoundCtx ->
  MixedVariableAssignment ->
  UserVariableCompilationStep ->
  m MixedVariableAssignment
applyReconstructionStep ctx assignment step = do
  logDebug MidDetail $ "Variable assignment:" <> line <> indent 2 (pretty assignment)
  case step of
    SolveEquality var eq -> solveEquality ctx assignment var eq
    SolveInequalities var solution -> solveInequalities ctx assignment var solution
    ReconstructUserTensor var elements -> constructUserTensorVariableFromElements ctx assignment var elements
    ReconstructNetworkTensor var elements -> constructNetworkTensorVariableFromElements ctx assignment var elements

solveEquality ::
  (MonadLogger m) =>
  NamedBoundCtx ->
  MixedVariableAssignment ->
  UserTensorVariable ->
  LinearExpr UserOrNetworkTensorVariable RatTensor ->
  m MixedVariableAssignment
solveEquality ctx assignment@VariableAssignment {..} var equality = do
  logCompilerSection MidDetail ("Reintroducing Gaussian-eliminated variable" <+> quotePretty var) $ do
    let value = handleMissingError ctx var $ evaluateExpr equality networkVariableValues
    logDebug MidDetail $ "Result:" <+> pretty var <+> "=" <+> pretty value
    return $
      assignment
        { userVariableValues = Map.insert var value userVariableValues
        }

solveInequalities ::
  (MonadLogger m) =>
  NamedBoundCtx ->
  MixedVariableAssignment ->
  UserTensorVariable ->
  Bounds UserOrNetworkTensorVariable RatTensor ->
  m MixedVariableAssignment
solveInequalities ctx assignment@VariableAssignment {..} var solution = do
  let doc = "Reintroducing Fourier-Motzkin-eliminated variable" <+> quotePretty var
  logCompilerSection MidDetail doc $ do
    let value = handleMissingError ctx var $ reconstructFourierMotzkinVariableValue networkVariableValues solution
    return $
      assignment
        { userVariableValues = Map.insert var value userVariableValues
        }

constructUserTensorVariableFromElements ::
  (MonadLogger m) =>
  NamedBoundCtx ->
  MixedVariableAssignment ->
  UserTensorVariable ->
  Tensor UserTensorVariable ->
  m MixedVariableAssignment
constructUserTensorVariableFromElements ctx assignment@VariableAssignment {..} variable elementVariables = do
  let doc = "Collapsing user variables" <+> pretty elementVariables <+> "to single variable" <+> pretty variable
  logCompilerSection MidDetail doc $ do
    let variableValue = handleMissingError ctx variable $ lookupElementVariables userVariableValues elementVariables
    let newUserVariableValues = Map.insert variable variableValue userVariableValues
    return $ assignment {userVariableValues = newUserVariableValues}

-- | Unreduces a previously reduced variable, removing the normalised
-- values from the assignment and adding the unreduced value back to the
-- assignment.
constructNetworkTensorVariableFromElements ::
  (MonadLogger m) =>
  NamedBoundCtx ->
  MixedVariableAssignment ->
  NetworkTensorVariable ->
  Tensor NetworkTensorVariable ->
  m MixedVariableAssignment
constructNetworkTensorVariableFromElements ctx assignment@VariableAssignment {..} var elementVariables = do
  let doc = "Collapsing network variables" <+> pretty elementVariables <+> "to single variable" <+> pretty variable
  logCompilerSection MidDetail doc $ do
    let variableValue = handleMissingError ctx var $ lookupElementVariables networkVariableValues elementVariables
    let newNetworkVariableValues = Map.insert var variableValue networkVariableValues
    return $ assignment {networkVariableValues = newNetworkVariableValues}

handleMissingError :: (VariableLike v1, VariableLike v2) => NamedBoundCtx -> v1 -> Either v2 a -> a
handleMissingError ctx var errorOrResult = case errorOrResult of
  Right result -> result
  Left missingVar -> do
    developerError $
      "When reconstructing variable"
        <+> prettyVariable var ctx
        <+> parens (pretty var)
        <+> "in counter-example,"
        <+> "unable to find variable"
        <+> prettyVariable missingVar ctx
        <+> parens (pretty missingVar)

prettyVariable :: (VariableLike v1) => v1 -> NamedBoundCtx -> Doc a
prettyVariable var ctx = pretty (lookupLvInBoundCtx (toLv var) ctx)

-- | Lookups the values in the variable assignment and removes them from the
-- assignment. Returns either the first missing variable or the list of values
-- and the resulting assignment.
lookupElementVariables ::
  (VariableLike variable) =>
  Map variable RatTensor ->
  Tensor variable ->
  Either variable (Tensor Rational)
lookupElementVariables values = traverse op
  where
    op var = case Map.lookup var values of
      Nothing -> Left var
      Just (ZeroDimTensor value) -> Right value
      Just _ -> developerError "Element variables should have an empty tensor shape"

createFinalAssignment ::
  (MonadLogger m) =>
  GenericBoundCtx Name ->
  MixedVariableAssignment ->
  m UserVariableAssignment
createFinalAssignment vehicleVariables (VariableAssignment {..}) = do
  let lookupName lv = lookupLvInBoundCtx lv vehicleVariables
  let stringVarAssignments = Map.mapKeys (lookupName . toLv) userVariableValues
  return $ UserVariableAssignment $ Map.toList stringVarAssignments
