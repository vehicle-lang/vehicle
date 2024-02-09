module Vehicle.Verify.Verifier.Test
  ( testVerifier,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text (pack, splitOn, strip)
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.QueryFormat.VNNLib (compileVNNLibVar)
import Vehicle.Verify.Variable
import Vehicle.Verify.Verifier.Core

-- This is a verifier only used for testing.
-- Ideally when we have a standard input/output/interaction format we won't need this anymore.

testVerifier :: Verifier
testVerifier =
  Verifier
    { verifierID = TestVerifier,
      verifierQueryFormatID = VNNLibQueries,
      verifierExecutableName = "testVerifier",
      prepareArgs = prepareTestVerifierArgs,
      parseOutput = parseTestVerifierOutput,
      supportsMultipleNetworkApplications = True
    }

prepareTestVerifierArgs :: PrepareVerifierArgs
prepareTestVerifierArgs metaNetwork queryFile = case networkEntries metaNetwork of
  [MetaNetworkEntry {..}] -> [networkFilepath metaNetworkEntryInfo, queryFile]
  _ -> developerError "Should have caught unsupported multiple network applications earlier"

parseTestVerifierOutput :: ParseVerifierOutput
parseTestVerifierOutput metaNetwork output = do
  let outputLines = fmap Text.pack (lines output)
  case outputLines of
    [] -> throwError $ VerifierOutputMalformed "No output lines"
    l : ls
      | l == "unsat" -> return $ UnSAT
      | otherwise -> do
          ioVarAssignment <- parseSATAssignment metaNetwork ls
          return $ SAT $ Just ioVarAssignment

parseSATAssignment ::
  (MonadError VerificationError m, MonadLogger m) =>
  MetaNetwork ->
  [Text] ->
  m NetworkVariableAssignment
parseSATAssignment metaNetwork ls = do
  let variableMap = Map.fromList $ fmap (\var -> (layoutAsText $ compileVNNLibVar var, var)) (variables metaNetwork)
  values <- traverse (parseSATAssignmentLine variableMap) ls
  return $ NetworkVariableAssignment $ Map.fromList values

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
        Nothing -> throwError $ VerifierOutputMalformed $ "Could not parse variable" <+> quotePretty namePart
    _ -> throwError $ VerifierOutputMalformed $ "Could not split assignment line" <+> quotePretty txt <+> "on '=' sign"
