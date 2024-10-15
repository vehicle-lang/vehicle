module Vehicle.Verify.Verifier.Test
  ( testVerifier,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text (pack, splitOn, strip)
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
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
prepareTestVerifierArgs metaNetwork queryFile = case metaNetwork of
  [MetaNetworkEntry {..}] -> [networkFilepath metaNetworkEntryInfo, queryFile]
  _ -> developerError "Should have caught unsupported multiple network applications earlier"

parseTestVerifierOutput :: ParseVerifierOutput
parseTestVerifierOutput output = do
  let outputLines = fmap Text.pack (lines output)
  case outputLines of
    [] -> throwError $ VerifierOutputMalformed "No output lines"
    l : ls
      | l == "unsat" -> return UnSAT
      | otherwise -> do
          ioVarAssignment <- parseSATAssignment ls
          return $ SAT $ Just ioVarAssignment

parseSATAssignment ::
  (MonadError VerificationError m, MonadLogger m) =>
  [Text] ->
  m QueryVariableAssignment
parseSATAssignment ls = do
  values <- traverse parseSATAssignmentLine ls
  return $ QueryVariableAssignment $ Map.fromList values

parseSATAssignmentLine ::
  (MonadError VerificationError m) =>
  Text ->
  m (QueryVariable, Rational)
parseSATAssignmentLine txt = do
  let parts = Text.strip <$> Text.splitOn "=" txt
  case parts of
    [namePart, valuePart] -> return (namePart, readFloatAsRational valuePart)
    _ -> throwError $ VerifierOutputMalformed $ "Could not split assignment line" <+> quotePretty txt <+> "on '=' sign"
