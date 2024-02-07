{-# LANGUAGE CPP #-}

module Vehicle.Verify.Verifier.Core where

import Control.Monad.Error.Class (MonadError (..))
import Vehicle.Compile.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.QueryFormat.Core
import Vehicle.Verify.Variable

# ifdef mingw32_HOST_OS
# else
import System.Posix.Signals
# endif

--------------------------------------------------------------------------------
-- Verifier interface

data VerifierID
  = Marabou
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

instance Pretty VerifierID where
  pretty = pretty . show

-- | Location of the verifier executable file
type VerifierExecutable = FilePath

-- | The type of methods that prepare the command line arguments for the verifier
type PrepareVerifierArgs =
  MetaNetwork -> QueryFile -> [String]

-- | The type of methods that parse the output of the verifier.
type ParseVerifierOutput =
  forall m. (MonadError VerificationError m) => MetaNetwork -> String -> m (QueryResult NetworkVariableAssignment)

-- | A complete verifier implementation
data Verifier = Verifier
  { -- | The identifier for the verifier within Vehicle itself
    verifierIdentifier :: VerifierID,
    -- | The query format that the verifier accepts
    verifierQueryFormat :: QueryFormatID,
    -- | The name of the executable for the verifier
    verifierExecutableName :: String,
    -- | Prepare the command line arguments for the verifier.
    prepareArgs :: PrepareVerifierArgs,
    -- | Parse the output of the verifier.
    parseOutput :: ParseVerifierOutput,
    -- | Does the verifier support multiple network applications?
    supportsMultipleNetworkApplications :: Bool
  }

data VerificationError
  = UnsupportedMultipleNetworks MetaNetwork
  | VerifierTerminatedByOS Int
  | VerifierError String
  | VerifierOutputMalformed (forall a. Doc a)

--------------------------------------------------------------------------------
-- Error messages

data VerificationErrorAction = VerificationErrorAction
  { reproducerIsUseful :: Bool,
    verificationErrorMessage :: forall a. Doc a
  }

convertVerificationError :: Verifier -> QueryAddress -> VerificationError -> VerificationErrorAction
convertVerificationError Verifier {..} (propertyAddress, queryID) = \case
  UnsupportedMultipleNetworks metaNetwork ->
    VerificationErrorAction
      { reproducerIsUseful = False,
        verificationErrorMessage = do
          let networkNames = fmap metaNetworkEntryName (networkEntries metaNetwork)
          let duplicateNetworkNames = findDuplicates networkNames
          "Marabou currently doesn't support properties that involve"
            <+> if null duplicateNetworkNames
              then
                "multiple networks. This property involves:"
                  <> line
                  <> indent 2 (vsep $ fmap (\n -> "the network" <+> squotes (pretty n)) networkNames)
              else
                "multiple applications of the same network. This property applies:"
                  <> line
                  <> indent 2 (vsep $ fmap (\(n, v) -> "the network" <+> squotes (pretty n) <+> pretty v <+> "times") duplicateNetworkNames)
      }
  VerifierError errorMessage ->
    VerificationErrorAction
      { reproducerIsUseful = True,
        verificationErrorMessage = do
          "while verifying query"
            <+> quotePretty queryID
            <+> "of property"
            <+> quotePretty propertyAddress
            <+> "the"
            <+> pretty verifierIdentifier
            <+> "verifier threw the error:"
            <> line
            <> line
            <> indent 2 (pretty errorMessage)
      }
  VerifierTerminatedByOS signal ->
    exitFailureReason verifierIdentifier signal
  VerifierOutputMalformed message ->
    VerificationErrorAction
      { reproducerIsUseful = False,
        verificationErrorMessage = "Unexpected output from Marabou..." <+> message
      }

exitFailureReason :: VerifierID -> Int -> VerificationErrorAction
# ifdef mingw32_HOST_OS
exitFailureReason verifierID exitValue = VerificationErrorAction
    { reproducerIsUseful = True
    , verificationErrorMessage = basicExitFailureMessage verifierID exitValue <+>
        "Vehicle is unable to interpret this error code on Windows but the most common reasons" <+>
        "are Marabou either ran out of memory or performed an illegal instruction."
    }
# else
exitFailureReason verifierID exitValue
  | toEnum exitValue == illegalInstruction = VerificationErrorAction
    { reproducerIsUseful = True
    , verificationErrorMessage = basicExitFailureMessage verifierID exitValue <+>
        "This is an `Illegal Instruction` error and indicates a bug in Marabou itself"
    }
  | toEnum exitValue == killProcess = VerificationErrorAction
    { reproducerIsUseful = True
    , verificationErrorMessage = basicExitFailureMessage verifierID exitValue <+>
        "This is often (but not always) a result of Marabou running out of memory."
    }
  | otherwise = VerificationErrorAction
    { reproducerIsUseful = True
    , verificationErrorMessage = basicExitFailureMessage verifierID exitValue <+>
        "Please consult the manual on Unix signals to work out what this means."
    }
# endif

basicExitFailureMessage :: VerifierID -> Int -> Doc a
basicExitFailureMessage verifierID exitValue =
  pretty verifierID
    <+> "was killed with the signal"
    <+> quotePretty exitValue
    <> "."
