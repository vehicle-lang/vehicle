module Vehicle.Verify.Solver.Test
  ( testSolver,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text (pack, splitOn, strip)
import Data.Version (Version (..))
import Vehicle.Compile.Prelude hiding (Solver)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Solver
import Vehicle.Verify.Specification.Status

-- This is a solver only used for testing.
-- Ideally when we have a standard input/output/interaction format we won't need this anymore.

testSolver :: SolverExecutable -> Solver
testSolver executable =
  Solver
    { solverName = "TestSolver",
      solverVersion = Version [1, 0] [],
      solverQueryFormatID = VNNLibQueries,
      solverExecutable = executable,
      prepareArgs = prepareTestSolverArgs,
      parseOutput = parseTestSolverOutput,
      supportsMultipleNetworkApplications = True
    }

prepareTestSolverArgs :: PrepareSolverArgs
prepareTestSolverArgs metaNetwork queryFile = case metaNetwork of
  [(_name, info, 1)] -> [networkFilepath info, queryFile]
  _ -> developerError "Should have caught unsupported multiple network applications earlier"

parseTestSolverOutput :: ParseSolverOutput
parseTestSolverOutput output = do
  let outputLines = fmap Text.pack (lines output)
  case outputLines of
    [] -> throwError $ SolverOutputMalformed "No output lines"
    l : ls
      | l == "unsat" -> return UnSAT
      | l == "timeout" -> throwError SolverTimedOut
      | otherwise -> SAT . Just <$> parseSATAssignment ls

parseSATAssignment ::
  (MonadError SolverError m, MonadLogger m) =>
  [Text] ->
  m QueryVariableAssignment
parseSATAssignment ls = do
  values <- traverse parseSATAssignmentLine ls
  return $ QueryVariableAssignment $ Map.fromList values

parseSATAssignmentLine ::
  (MonadError SolverError m) =>
  Text ->
  m (QueryVariable, Rational)
parseSATAssignmentLine txt = do
  let parts = Text.strip <$> Text.splitOn "=" txt
  case parts of
    [namePart, valuePart] -> return (namePart, readFloatAsRational valuePart)
    _ -> throwError $ SolverOutputMalformed $ "Could not split assignment line" <+> quotePretty txt <+> "on '=' sign"
