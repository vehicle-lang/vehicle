module Vehicle.Verify.Verifier.Marabou
  ( marabouVerifier,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (elemIndex, findIndex)
import Data.Text (Text, pack)
import Data.Text qualified as Text (pack, splitOn, strip, unpack)
import Data.Text.IO (hPutStrLn)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import System.Exit (ExitCode (..), exitFailure)
import System.IO (stderr)
import System.Process (readProcessWithExitCode)
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core

--------------------------------------------------------------------------------
-- The main interface

marabouVerifier :: Verifier
marabouVerifier =
  Verifier
    { verifierIdentifier = Marabou,
      verifierExecutableName = "Marabou",
      invokeVerifier = invokeMarabou,
      verifierQueryFormat = MarabouQueryFormat
    }

--------------------------------------------------------------------------------
-- Invoking Marabou

invokeMarabou :: VerifierInvocation
invokeMarabou marabouExecutable networkLocations queryFile =
  liftIO $ do
    networkArg <- prepareNetworkArg networkLocations
    marabouOutput <- readProcessWithExitCode marabouExecutable [networkArg, queryFile] ""
    parseMarabouOutput marabouOutput

prepareNetworkArg :: MetaNetwork -> IO String
prepareNetworkArg [(_name, file)] = return file
prepareNetworkArg _ = do
  hPutStrLn stderr $
    "Marabou currently doesn't support properties that involve"
      <> "multiple neural networks or multiple applications of the same network."
  exitFailure

parseMarabouOutput :: (ExitCode, String, String) -> IO (QueryResult NetworkVariableCounterexample)
parseMarabouOutput (exitCode, out, _err) = case exitCode of
  ExitFailure _ -> do
    -- Marabou seems to output its error messages to stdout rather than stderr...
    hPutStrLn
      stderr
      ( "Marabou threw the following error:\n"
          <> "  "
          <> pack out
      )
    exitFailure
  ExitSuccess -> do
    let outputLines = fmap Text.pack (lines out)
    let resultIndex = findIndex (\v -> v == "sat" || v == "unsat") outputLines
    case resultIndex of
      Nothing -> malformedOutputError "cannot find 'sat' or 'unsat'"
      Just i
        | outputLines !! i == "unsat" ->
            return UnSAT
        | otherwise -> do
            let assignmentOutput = drop (i + 1) outputLines
            let ioVarAssignment = parseSATAssignment assignmentOutput
            return $ SAT $ Just ioVarAssignment

-- TODO reverse normalisation of quantified user tensor variables
-- Just _x -> return $ SAT Nothing

parseSATAssignment :: [Text] -> Vector Double
parseSATAssignment output = do
  let mInputIndex = elemIndex "Input assignment:" output
  let mOutputIndex = elemIndex "Output:" output
  case (mInputIndex, mOutputIndex) of
    (Just inputIndex, Just outputIndex) -> do
      let inputVarLines = take (outputIndex - inputIndex - 1) $ drop (inputIndex + 1) output
      let outputVarLines = drop (outputIndex + 1) output
      let inputValues = parseSATAssignmentLine Input <$> inputVarLines
      let outputValues = parseSATAssignmentLine Output <$> outputVarLines
      Vector.fromList (inputValues <> outputValues)
    _ -> malformedOutputError "could not find strings 'Input assignment:' and 'Output:'"

parseSATAssignmentLine :: InputOrOutput -> Text -> Double
parseSATAssignmentLine _ txt =
  let parts = Text.strip <$> Text.splitOn "=" txt
   in case parts of
        [_namePart, valuePart] -> read (Text.unpack valuePart)
        _ -> malformedOutputError "could not split assignment line on '=' sign"

malformedOutputError :: Doc a -> b
malformedOutputError x =
  error $ layoutAsString ("Unexpected output from Marabou..." <+> x)
