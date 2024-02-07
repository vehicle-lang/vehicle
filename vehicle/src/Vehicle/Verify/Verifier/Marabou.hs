module Vehicle.Verify.Verifier.Marabou
  ( marabouVerifier,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.List (elemIndex, findIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text (pack, splitOn, strip)
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.Marabou (compileVar)
import Vehicle.Verify.Variable
import Vehicle.Verify.Verifier.Core

--------------------------------------------------------------------------------
-- The main interface

marabouVerifier :: Verifier
marabouVerifier =
  Verifier
    { verifierIdentifier = Marabou,
      verifierExecutableName = "Marabou",
      verifierQueryFormat = MarabouQueries,
      prepareArgs = prepareMarabouArgs,
      parseOutput = parseMarabouOutput,
      -- Marabou seems to output its error messages to stdout rather than stderr...
      -- https://github.com/NeuralNetworkVerification/Marabou/issues/714
      supportsMultipleNetworkApplications = False
    }

prepareMarabouArgs :: PrepareVerifierArgs
prepareMarabouArgs metaNetwork queryFile = case networkEntries metaNetwork of
  [MetaNetworkEntry {..}] -> [networkFilepath metaNetworkEntryInfo, queryFile]
  _ -> developerError "Should have caught unsupported multiple network applications earlier"

parseMarabouOutput :: ParseVerifierOutput
parseMarabouOutput metaNetwork out = do
  let outputLines = fmap Text.pack (lines out)
  let resultIndex = findIndex (\v -> v == "sat" || v == "unsat") outputLines
  case resultIndex of
    Nothing -> throwError $ VerifierOutputMalformed "cannot find 'sat' or 'unsat'"
    Just i
      | outputLines !! i == "unsat" -> return $ UnSAT
      | otherwise -> do
          let assignmentOutput = drop (i + 1) outputLines
          ioVarAssignment <- parseSATAssignment metaNetwork (filter (/= "") assignmentOutput)
          return $ SAT $ Just ioVarAssignment

parseSATAssignment ::
  (MonadError VerificationError m) =>
  MetaNetwork ->
  [Text] ->
  m NetworkVariableAssignment
parseSATAssignment metaNetwork output = do
  let variableMap = Map.fromList $ fmap (\var -> (layoutAsText $ compileVar var, var)) (variables metaNetwork)
  let mInputIndex = elemIndex "Input assignment:" output
  let mOutputIndex = elemIndex "Output:" output
  case (mInputIndex, mOutputIndex) of
    (Just inputIndex, Just outputIndex) -> do
      let inputVarLines = take (outputIndex - inputIndex - 1) $ drop (inputIndex + 1) output
      let outputVarLines = drop (outputIndex + 1) output
      values <- traverse (parseSATAssignmentLine variableMap) (inputVarLines <> outputVarLines)
      return $ NetworkVariableAssignment $ Map.fromList values
    _ -> throwError $ VerifierOutputMalformed "could not find strings 'Input assignment:' and 'Output:'"

parseSATAssignmentLine ::
  (MonadError VerificationError m) =>
  Map Text NetworkRationalVariable ->
  Text ->
  m (NetworkRationalVariable, Rational)
parseSATAssignmentLine varsByName txt = do
  let parts = Text.strip <$> Text.splitOn "=" txt
  case parts of
    [namePart, valuePart] -> do
      case Map.lookup namePart varsByName of
        Just var -> return (var, readFloatAsRational valuePart)
        Nothing -> throwError $ VerifierOutputMalformed $ "could not parse variable" <+> quotePretty namePart
    _ -> throwError $ VerifierOutputMalformed $ "could not split assignment line" <+> quotePretty txt <+> "on '=' sign"
