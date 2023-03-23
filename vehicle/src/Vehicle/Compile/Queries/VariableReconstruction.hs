module Vehicle.Compile.Queries.VariableReconstruction where

import Control.Monad (foldM)
import Data.Vector.Unboxed qualified as Vector
import Vehicle.Compile.Queries.FourierMotzkinElimination
import Vehicle.Compile.Queries.GaussianElimination
import Vehicle.Compile.Queries.LinearExpr
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- Variable reconstruction

reconstructUserVars ::
  UserVarReconstructionInfo ->
  VariableAssignment ->
  Maybe VariableAssignment
reconstructUserVars info ioVarValues = do
  let numberOfUserVars = length info
  let startingUserVarValues = Vector.replicate numberOfUserVars 0
  let constantValue = Vector.singleton 1.0
  let startingValues = startingUserVarValues <> ioVarValues <> constantValue
  let reconstructedVars = foldM reconstructVariable startingValues info
  Vector.take numberOfUserVars <$> reconstructedVars

reconstructVariable ::
  VariableAssignment ->
  (LinearVar, VariableSolution) ->
  Maybe VariableAssignment
reconstructVariable assignment (var, solution) = do
  value <- case solution of
    GaussianSolution sol ->
      reconstructGaussianVariableValue assignment sol
    FourierMotzkinSolution sol ->
      reconstructFourierMotzkinVariableValue assignment sol
  return $ Vector.update assignment [(var, value)]
