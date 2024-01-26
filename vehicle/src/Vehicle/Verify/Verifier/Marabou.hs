module Vehicle.Verify.Verifier.Marabou
  ( marabouVerifier,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (elemIndex, findIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text (pack, splitOn, strip)
import Data.Text.IO (hPutStrLn)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (stderr)
import System.Process (readProcessWithExitCode)
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
      invokeVerifier = invokeMarabou,
      verifierQueryFormat = MarabouQueries
    }

--------------------------------------------------------------------------------
-- Invoking Marabou

invokeMarabou :: VerifierInvocation
invokeMarabou marabouExecutable metaNetwork queryFile = do
  -- Prepare arguments
  networkArg <- prepareNetworkArg (networkEntries metaNetwork)
  let args = [networkArg, queryFile]
  let command = unwords (marabouExecutable : args)

  -- Run command
  logDebug MaxDetail $ "Running verify command: " <> pretty command
  (exitCode, out, err) <- liftIO $ readProcessWithExitCode marabouExecutable args ""

  -- Parse result
  logDebug MaxDetail $ "Output of verify command: " <> line <> indent 2 (pretty out)
  parseMarabouOutput metaNetwork command (exitCode, out, err)

prepareNetworkArg :: (MonadIO m) => [MetaNetworkEntry] -> m String
prepareNetworkArg [MetaNetworkEntry {..}] = return (networkFilepath metaNetworkEntryInfo)
prepareNetworkArg metaNetwork = do
  let duplicateNetworkNames = findDuplicates (fmap metaNetworkEntryName metaNetwork)

  let errorMsg =
        "Error: Marabou currently doesn't support properties that involve"
          <+> if null duplicateNetworkNames
            then
              "multiple networks. This property involves:"
                <> line
                <> indent 2 (vsep $ fmap (\e -> "the network" <+> squotes (pretty $ metaNetworkEntryName e)) metaNetwork)
            else
              "multiple applications of the same network. This property applies:"
                <> line
                <> indent 2 (vsep $ fmap (\(n, v) -> "the network" <+> squotes (pretty n) <+> pretty v <+> "times") duplicateNetworkNames)

  liftIO $ do
    hPutStrLn stderr $ layoutAsText errorMsg
    exitFailure

parseMarabouOutput ::
  (MonadIO m) =>
  MetaNetwork ->
  String ->
  (ExitCode, String, String) ->
  m (Either Text (QueryResult NetworkVariableAssignment))
parseMarabouOutput metaNetwork command (exitCode, out, _err) = case exitCode of
  ExitFailure exitValue
    | exitValue < 0 -> do
        -- Marabou was killed by the system.
        -- See System.Process.html#waitForProcess documentation
        let errorDoc =
              "Marabou was automatically killed by the OS with signal"
                <+> quotePretty (-exitValue)
                <> "."
                <> line
                <> "The most common reason for this is an out-of-memory-error."
        return $ Left $ layoutAsText errorDoc
    | otherwise -> do
        -- Marabou seems to output its error messages to stdout rather than stderr...
        let errorDoc =
              "Marabou threw the following error:"
                <> line
                <> indent 2 (pretty out)
                <> line
                <> "when running the command:"
                <> line
                <> indent 2 (pretty command)
        liftIO $ do
          hPutStrLn stderr (layoutAsText errorDoc)
          exitFailure
  ExitSuccess -> do
    let outputLines = fmap Text.pack (lines out)
    let resultIndex = findIndex (\v -> v == "sat" || v == "unsat") outputLines
    case resultIndex of
      Nothing -> malformedOutwriteStderror "cannot find 'sat' or 'unsat'"
      Just i
        | outputLines !! i == "unsat" ->
            return $ Right UnSAT
        | otherwise -> do
            let assignmentOutput = drop (i + 1) outputLines
            ioVarAssignment <- parseSATAssignment metaNetwork (filter (/= "") assignmentOutput)
            return $ Right $ SAT $ Just ioVarAssignment

parseSATAssignment :: (MonadIO m) => MetaNetwork -> [Text] -> m NetworkVariableAssignment
parseSATAssignment metaNetwork output = do
  let variableMap = Map.fromList $ fmap (\var -> (layoutAsText $ compileVar var, var)) (variables metaNetwork)
  let mInputIndex = elemIndex "Input assignment:" output
  let mOutputIndex = elemIndex "Output:" output
  case (mInputIndex, mOutputIndex) of
    (Just inputIndex, Just outputIndex) -> do
      let inputVarLines = take (outputIndex - inputIndex - 1) $ drop (inputIndex + 1) output
      let outputVarLines = drop (outputIndex + 1) output
      let values = parseSATAssignmentLine variableMap <$> (inputVarLines <> outputVarLines)
      return $ NetworkVariableAssignment $ Map.fromList values
    _ -> malformedOutwriteStderror "could not find strings 'Input assignment:' and 'Output:'"

parseSATAssignmentLine ::
  Map Text NetworkRationalVariable ->
  Text ->
  (NetworkRationalVariable, Rational)
parseSATAssignmentLine varsByName txt =
  let parts = Text.strip <$> Text.splitOn "=" txt
   in case parts of
        [namePart, valuePart] -> do
          let var = case Map.lookup namePart varsByName of
                Just v -> v
                Nothing -> malformedOutwriteStderror $ "could not parse variable" <+> quotePretty namePart
          let value = readFloatAsRational valuePart
          (var, value)
        _ ->
          malformedOutwriteStderror $
            "could not split assignment line" <+> quotePretty txt <+> "on '=' sign"

malformedOutwriteStderror :: Doc a -> b
malformedOutwriteStderror x =
  error $ layoutAsString ("Unexpected output from Marabou..." <+> x)
