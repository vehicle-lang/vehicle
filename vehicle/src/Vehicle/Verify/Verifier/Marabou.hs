module Vehicle.Verify.Verifier.Marabou
  ( marabouVerifier
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (elemIndex, findIndex)
import Data.Map qualified as Map (lookup)
import Data.Text (Text, pack)
import Data.Text qualified as Text (pack, splitOn, strip, unpack)
import Data.Text.IO (hPutStrLn)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector
import System.Exit (ExitCode (..), exitFailure)
import System.IO (stderr)
import System.Process (readProcessWithExitCode)
import Vehicle.Compile.Linearity
import Vehicle.Compile.Prelude
import Vehicle.Compile.Resource
import Vehicle.Verify.Core
import Vehicle.Verify.Specification.Status
import Vehicle.Verify.Verifier.Interface

--------------------------------------------------------------------------------
-- The main interface

marabouVerifier :: Verifier
marabouVerifier = Verifier
  { verifierIdentifier     = Marabou
  , verifierExecutableName = "Marabou"
  , invokeVerifier         = invokeMarabou
  , compileQuery           = compileMarabouQuery
  , magicVariablePrefixes  = ("x", "y")
  }

--------------------------------------------------------------------------------
-- Compiling to Marabou

-- | Compiles an expression representing a single Marabou query. The expression
-- passed should only have conjunctions and existential quantifiers at the boolean
-- level.
compileMarabouQuery :: MonadLogger m => CLSTProblem -> m (Doc ())
compileMarabouQuery (CLSTProblem varNames assertions) = do
  assertionDocs <- forM assertions (compileAssertion varNames)
  let assertionsDoc = vsep assertionDocs
  return assertionsDoc

compileAssertion :: MonadLogger m
                 => VariableNames
                 -> Assertion
                 -> m (Doc a)
compileAssertion varNames (Assertion rel linearExpr) = do
  let (coefficientsVec, constant) = splitOutConstant linearExpr
  let coefficients = Vector.toList coefficientsVec
  let allCoeffVars = zip coefficients varNames
  let coeffVars = filter (\(c,_) -> c /= 0) allCoeffVars

  -- Make the properties a tiny bit nicer by checking if all the vars are
  -- negative and if so negating everything.
  let allCoefficientsNegative = all (\(c,_) -> c < 0) coeffVars
  let (finalCoefVars, constant', flipRel) = if allCoefficientsNegative
        then (fmap (\(c,n) -> (-c,n)) coeffVars, -constant, True)
        else (coeffVars, constant, False)

  -- Marabou always has the constants on the RHS so we need to negate the constant.
  let negatedConstant = -constant'
  -- Also check for and remove `-0.0`s for cleanliness.
  let finalConstant = if isNegativeZero negatedConstant then 0.0 else negatedConstant

  let compiledRel = compileRel flipRel rel
  let compiledLHS = hsep (fmap (compileVar (length finalCoefVars > 1)) finalCoefVars)
  let compiledRHS = pretty finalConstant
  return $ compiledLHS <+> compiledRel <+> compiledRHS

compileRel :: Bool -> Relation -> Doc a
compileRel _     Equal             = "="
compileRel False LessThanOrEqualTo = "<="
compileRel True  LessThanOrEqualTo = ">="
-- Suboptimal. Marabou doesn't currently support strict inequalities.
-- See https://github.com/vehicle-lang/vehicle/issues/74 for details.
compileRel False LessThan          = "<="
compileRel True  LessThan          = ">="

compileVar :: Bool -> (Double, Name) -> Doc a
compileVar False (1,           var) = pretty var
compileVar True  (1,           var) = "+" <> pretty var
compileVar _     (-1,          var) = "-" <> pretty var
compileVar _     (coefficient, var) = pretty coefficient <> pretty var

--------------------------------------------------------------------------------
-- Invoking Marabou

invokeMarabou :: VerifierInvocation
invokeMarabou marabouExecutable metaNetwork networkLocations varReconstruction queryFile =
  liftIO $ do
    networkArg <- prepareNetworkArg metaNetwork networkLocations
    marabouOutput <- readProcessWithExitCode marabouExecutable [networkArg, queryFile] ""
    parseMarabouOutput varReconstruction marabouOutput

prepareNetworkArg :: NetworkLocations -> MetaNetwork -> IO String
prepareNetworkArg networkLocations [name] =
  case Map.lookup name networkLocations of
    Just path -> return path
    _ -> do
      hPutStrLn stderr $
        "No file provided for neural network '" <> name <> "'. " <>
        "Please provide it via the '--network' command line option."
      exitFailure
prepareNetworkArg _ _ = do
  hPutStrLn stderr $
    "Marabou currently doesn't support properties that involve" <>
    "multiple neural networks or multiple applications of the same network."
  exitFailure

parseMarabouOutput :: UserVarReconstructionInfo
                   -> (ExitCode, String, String)
                   -> IO SatisfiabilityStatus
parseMarabouOutput reconstructionInfo (exitCode, out, _err) = case exitCode of
  ExitFailure _ -> do
    -- Marabou seems to output its error messages to stdout rather than stderr...
    hPutStrLn stderr
      ("Marabou threw the following error:\n" <>
      "  " <> pack out)
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
          let assignmentOutput = drop (i+1) outputLines
          let ioVarAssignment = parseSATAssignment assignmentOutput
          let maybeLinearVars = reconstructUserVars reconstructionInfo ioVarAssignment
          case maybeLinearVars of
            Nothing -> return $ SAT Nothing
            -- TODO reverse normalisation of quantified user tensor variables
            Just _x -> return $ SAT Nothing

parseSATAssignment :: [Text] -> Vector Double
parseSATAssignment output =
  let mInputIndex  = elemIndex "Input assignment:" output in
  let mOutputIndex = elemIndex "Output:" output in
  case (mInputIndex, mOutputIndex) of
    (Just inputIndex, Just outputIndex) ->
      let inputVarLines = take (outputIndex - inputIndex - 1) $ drop (inputIndex + 1) output in
      let outputVarLines = drop (outputIndex + 1) output in
      let inputValues  = parseSATAssignmentLine Input  <$> inputVarLines  in
      let outputValues = parseSATAssignmentLine Output <$> outputVarLines in
      Vector.fromList (inputValues <> outputValues)
    _ -> malformedOutputError "could not find strings 'Input assignment:' and 'Output:'"

parseSATAssignmentLine :: InputOrOutput -> Text -> Double
parseSATAssignmentLine _ txt =
  let parts = Text.strip <$> Text.splitOn "=" txt in
  case parts of
    [_namePart, valuePart] -> read (Text.unpack valuePart)
    _                      -> malformedOutputError "could not split assignment line on '=' sign"

malformedOutputError :: Doc a -> b
malformedOutputError x =
  error $ layoutAsString ("Unexpected output from Marabou..." <+> x)
