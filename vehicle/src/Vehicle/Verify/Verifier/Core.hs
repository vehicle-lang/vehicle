{-# LANGUAGE CPP #-}

module Vehicle.Verify.Verifier.Core where

# ifdef mingw32_HOST_OS
# else
import System.Posix.Signals
# endif

import Control.Monad.Error.Class (MonadError (..))
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core

--------------------------------------------------------------------------------
-- Verifier interface

data VerifierID
  = Marabou
  | TestVerifier
  deriving (Eq, Ord, Read, Bounded, Enum)

instance Show VerifierID where
  show = \case
    Marabou -> "Marabou"
    TestVerifier -> "TestVerifier"

instance Pretty VerifierID where
  pretty = pretty . show

-- | Location of the verifier executable file
type VerifierExecutable = FilePath

-- | The type of methods that prepare the command line arguments for the verifier
type PrepareVerifierArgs = MetaNetwork -> QueryFile -> [String]

data SolverResult
  = ReturnedSAT (Maybe QueryVariablesAssignment)
  | ReturnedUnSAT
  | TimedOut
  | ReturnedUnknown

-- | The type of methods that parse the output of the verifier.
type ParseVerifierOutput =
  forall m.
  (MonadError VerifierError m, MonadLogger m) =>
  String ->
  m SolverResult

-- | A complete verifier implementation
data Verifier = Verifier
  { -- | The identifier for the verifier within Vehicle itself
    verifierID :: VerifierID,
    -- | The query format that the verifier accepts
    verifierQueryFormatID :: QueryFormatID,
    -- | The name of the executable for the verifier
    verifierExecutableName :: String,
    -- | Prepare the command line arguments for the verifier.
    prepareArgs :: PrepareVerifierArgs,
    -- | Parse the output of the verifier.
    parseOutput :: ParseVerifierOutput,
    -- | Does the verifier support multiple network applications?
    supportsMultipleNetworkApplications :: Bool
  }

--------------------------------------------------------------------------------
-- Error messages

printVerifierError :: Verifier -> QueryAddress -> VerifierError -> Doc ()
printVerifierError Verifier {..} (QueryAddress propertyAddress queryID) = \case
  VerifierError errorMessage -> do
    "while verifying query"
      <+> pretty queryID
      <+> "of property"
      <+> quotePretty propertyAddress
      <+> "the"
      <+> verifierDoc
      <+> "threw the error:"
      <> line
      <> line
      <> indent 2 (pretty errorMessage)
  VerifierTerminatedByOS signal ->
    exitFailureReason signal
  VerifierOutputMalformed message ->
    "Unexpected output from the" <+> verifierDoc <> "." <+> pretty message
  VerifierIncompleteWitness missingVariables ->
    "The witness provided from the"
      <+> verifierDoc
      <+> "was incomplete."
      <+> "In particular, values for the following variables were not provided:"
      <> line
      <> indent 2 (prettySet pretty missingVariables)
  where
    verifierDoc = pretty verifierID <+> "verifier"

    exitFailureReason :: Int -> Doc ()
# ifdef mingw32_HOST_OS
    exitFailureReason exitValue =
        "Vehicle is unable to interpret this error code on Windows but the most common reasons" <+>
            "are the" <+> verifierDoc <+> "either ran out of memory or performed an illegal instruction."
# else
    exitFailureReason exitValue
      | toEnum exitValue == illegalInstruction =
          basicExitFailureMessage verifierID exitValue <+>
            "This is an `Illegal Instruction` error and indicates a bug in the" <+> verifierDoc <+> "itself"
      | toEnum exitValue == killProcess =
          basicExitFailureMessage verifierID exitValue <+>
            "This is often (but not always) a result of the" <+> verifierDoc <+> "running out of memory."
      | otherwise =
          basicExitFailureMessage verifierID exitValue <+>
            "Please consult the manual on Unix signals to work out what this means."
# endif

basicExitFailureMessage :: VerifierID -> Int -> Doc a
basicExitFailureMessage verifierID exitValue =
  pretty verifierID
    <+> "was killed with the signal"
    <+> quotePretty exitValue
    <> "."
