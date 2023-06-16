module Vehicle.Compile.Queries.VariableReconstruction where

import Data.Vector.Unboxed qualified as Vector
import Vehicle.Compile.Queries.FourierMotzkinElimination
import Vehicle.Compile.Queries.GaussianElimination
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Prelude
import Vehicle.Syntax.AST (Name)
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Variable reconstruction

reconstructUserVars ::
  QueryVariableInfo ->
  NetworkVariableAssignments ->
  UserVariableAssignments
reconstructUserVars QueryVariableInfo {..} networkVariableAssignment = do
  let normalisedVariableAssignment =
        reconstructNormalisedVariables normalisedVariableInfo networkVariableAssignment

  let unnormalisedVariableAssignment =
        reconstructUnnormalisedVariables unnormalisedVariableInfo normalisedVariableAssignment

  unnormalisedVariableAssignment

reconstructUnnormalisedVariables ::
  QueryUnnormalisedVariableInfo ->
  VariableAssignment ->
  UserVariableAssignments
reconstructUnnormalisedVariables info normalisedVariableAssignment = do
  fst $ foldl reconstructUnnormalisedVariable ([], normalisedVariableAssignment) info

reconstructUnnormalisedVariable ::
  (UserVariableAssignments, VariableAssignment) ->
  (Name, TensorDimensions) ->
  (UserVariableAssignments, VariableAssignment)
reconstructUnnormalisedVariable (counterexamples, assignment) (name, dims) = do
  let (varAssignment, assignmentRemainder) = Vector.splitAt (product dims) assignment
  let counterexample = UserVariableAssignment name dims varAssignment
  (counterexample : counterexamples, assignmentRemainder)

reconstructNormalisedVariables ::
  QueryNormalisedVariableInfo ->
  NetworkVariableAssignments ->
  VariableAssignment
reconstructNormalisedVariables info networkVariableAssignment = do
  let numberOfUserVars = length info
  let startingUserVarValues = Vector.replicate numberOfUserVars 0
  let constantValue = Vector.singleton 1.0
  let startingValues = startingUserVarValues <> networkVariableAssignment <> constantValue
  let finalValues = foldl reconstructNormalisedVariable startingValues info
  Vector.take numberOfUserVars finalValues

reconstructNormalisedVariable ::
  VariableAssignment ->
  (LinearVar, VariableSolution) ->
  VariableAssignment
reconstructNormalisedVariable assignment (var, solution) = do
  let value =
        case solution of
          GaussianSolution sol ->
            reconstructGaussianVariableValue assignment sol
          FourierMotzkinSolution sol ->
            reconstructFourierMotzkinVariableValue assignment sol
  Vector.update assignment [(var, value)]
