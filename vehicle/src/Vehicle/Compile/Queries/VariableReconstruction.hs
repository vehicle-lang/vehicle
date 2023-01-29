module Vehicle.Compile.Queries.VariableReconstruction where

import Control.Monad (foldM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Vector.Unboxed qualified as Vector
import GHC.Generics (Generic)
import Vehicle.Compile.Queries.FourierMotzkinElimination
  ( FourierMotzkinVariableSolution,
    reconstructFourierMotzkinVariableValue,
  )
import Vehicle.Compile.Queries.GaussianElimination
  ( GaussianVariableSolution,
    reconstructGaussianVariableValue,
  )
import Vehicle.Compile.Queries.LinearExpr

--------------------------------------------------------------------------------
-- Variable reconstruction

-- | Information neccesary to reconstruct the user variables from the magic
-- input/output variables.
data VariableSolution
  = GaussianSolution GaussianVariableSolution
  | FourierMotzkinSolution FourierMotzkinVariableSolution
  deriving (Generic)

instance ToJSON VariableSolution

instance FromJSON VariableSolution

type UserVarReconstructionInfo = [(LinearVar, VariableSolution)]

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
