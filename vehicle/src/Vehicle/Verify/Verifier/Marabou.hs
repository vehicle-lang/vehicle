module Vehicle.Verify.Verifier.Marabou
  ( marabouVerifier,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.List (elemIndex, findIndex)
import Data.Text (Text)
import Data.Text qualified as Text (pack, splitOn, strip)
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Verifier.Core

--------------------------------------------------------------------------------
-- The main interface

marabouVerifier :: Verifier
marabouVerifier =
  Verifier
    { verifierID = Marabou,
      verifierQueryFormatID = MarabouQueries,
      verifierExecutableName = "Marabou",
      prepareArgs = prepareMarabouArgs,
      parseOutput = parseMarabouOutput,
      supportsMultipleNetworkApplications = False
    }

prepareMarabouArgs :: PrepareVerifierArgs
prepareMarabouArgs metaNetwork queryFile = case metaNetwork of
  [(_name, info, 1)] -> [networkFilepath info, queryFile]
  _ -> developerError "Should have caught unsupported multiple network applications earlier"

parseMarabouOutput :: ParseVerifierOutput
parseMarabouOutput output = do
  let outputLines = fmap Text.pack (lines output)
  let resultIndex = findIndex (\v -> v == "sat" || v == "unsat" || v == "Timeout") outputLines
  case resultIndex of
    Nothing -> do
      logDebug MinDetail $ pretty output
      throwError $ VerifierOutputMalformed "Cannot find 'sat', 'unsat' or 'timeout'"
    Just i
      | outputLines !! i == "Timeout" -> return TimedOut
      | outputLines !! i == "unsat" -> return ReturnedUnSAT
      | otherwise -> do
          let assignmentOutput = drop (i + 1) outputLines
          ioVarAssignment <- parseSATAssignment (filter (/= "") assignmentOutput)
          return $ ReturnedSAT $ Just ioVarAssignment

parseSATAssignment ::
  (MonadError VerifierError m) =>
  [Text] ->
  m QueryVariablesAssignment
parseSATAssignment output = do
  -- let variableMap = Map.fromList $ fmap (\var -> (layoutAsText $ compileMarabouVar var, var)) (variables metaNetwork)
  let mInputIndex = elemIndex "Input assignment:" output
  let mOutputIndex = elemIndex "Output:" output
  case (mInputIndex, mOutputIndex) of
    (Just inputIndex, Just outputIndex) -> do
      let inputVarLines = take (outputIndex - inputIndex - 1) $ drop (inputIndex + 1) output
      let outputVarLines = drop (outputIndex + 1) output
      traverse parseSATAssignmentLine (inputVarLines <> outputVarLines)
    _ -> throwError $ VerifierOutputMalformed "Could not find strings 'Input assignment:' and 'Output:'"

parseSATAssignmentLine ::
  (MonadError VerifierError m) =>
  Text ->
  m (QueryVariable, Rational)
parseSATAssignmentLine txt = do
  let parts = Text.strip <$> Text.splitOn "=" txt
  case parts of
    [namePart, valuePart] -> return (namePart, readFloatAsRational valuePart)
    _ -> throwError $ VerifierOutputMalformed $ layoutAsString $ "Could not split assignment line" <+> quotePretty txt <+> "on '=' sign"
