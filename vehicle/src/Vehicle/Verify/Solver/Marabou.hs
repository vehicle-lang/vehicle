module Vehicle.Verify.Solver.Marabou
  ( marabouSolver,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.List (elemIndex, findIndex)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text (pack, splitOn, strip)
import Data.Version (Version (..))
import Vehicle.Compile.Prelude hiding (Solver)
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Solver
import Vehicle.Verify.Specification.Status

--------------------------------------------------------------------------------
-- The main interface

marabouSolver :: SolverExecutable -> Solver
marabouSolver executable =
  Solver
    { solverName = "Marabou",
      solverVersion = Version [2, 0] [],
      solverQueryFormatID = MarabouQueries,
      solverExecutable = executable,
      prepareArgs = prepareMarabouArgs,
      parseOutput = parseMarabouOutput,
      supportsMultipleNetworkApplications = False
    }

prepareMarabouArgs :: PrepareSolverArgs
prepareMarabouArgs metaNetwork queryFile = case metaNetwork of
  [(_name, info, 1)] -> [networkFilepath info, queryFile]
  _ -> developerError "Should have caught unsupported multiple network applications earlier"

parseMarabouOutput :: ParseSolverOutput
parseMarabouOutput output = do
  let outputLines = fmap Text.pack (lines output)
  let resultIndex = findIndex (\v -> v == "sat" || v == "unsat" || v == "Timeout") outputLines
  case resultIndex of
    Nothing -> do
      logDebug MinDetail $ pretty output
      throwError $ SolverOutputMalformed "Cannot find 'sat', 'unsat' or 'timeout'"
    Just i
      | outputLines !! i == "Timeout" -> throwError SolverTimedOut
      | outputLines !! i == "unsat" -> return UnSAT
      | otherwise -> do
          let assignmentOutput = drop (i + 1) outputLines
          ioVarAssignment <- parseSATAssignment (filter (/= "") assignmentOutput)
          return $ SAT $ Just ioVarAssignment

parseSATAssignment ::
  (MonadError SolverError m) =>
  [Text] ->
  m QueryVariableAssignment
parseSATAssignment output = do
  -- let variableMap = Map.fromList $ fmap (\var -> (layoutAsText $ compileMarabouVar var, var)) (variables metaNetwork)
  let mInputIndex = elemIndex "Input assignment:" output
  let mOutputIndex = elemIndex "Output:" output
  case (mInputIndex, mOutputIndex) of
    (Just inputIndex, Just outputIndex) -> do
      let inputVarLines = take (outputIndex - inputIndex - 1) $ drop (inputIndex + 1) output
      let outputVarLines = drop (outputIndex + 1) output
      values <- traverse parseSATAssignmentLine (inputVarLines <> outputVarLines)
      return $ QueryVariableAssignment $ Map.fromList values
    _ -> throwError $ SolverOutputMalformed "Could not find strings 'Input assignment:' and 'Output:'"

parseSATAssignmentLine ::
  (MonadError SolverError m) =>
  Text ->
  m (QueryVariable, Rational)
parseSATAssignmentLine txt = do
  let parts = Text.strip <$> Text.splitOn "=" txt
  case parts of
    [namePart, valuePart] -> return (namePart, readFloatAsRational valuePart)
    _ -> throwError $ SolverOutputMalformed $ "Could not split assignment line" <+> quotePretty txt <+> "on '=' sign"
