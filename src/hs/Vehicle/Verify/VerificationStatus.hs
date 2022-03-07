module Vehicle.Verify.VerificationStatus where

import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy qualified as ByteString
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Data.Version (Version)

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Network verification information

data NetworkVerificationInfo = NetworkVerificationInfo
  { name        :: Text
  , location    :: FilePath
  , networkHash :: Int
  } deriving (Generic)

instance FromJSON NetworkVerificationInfo
instance ToJSON NetworkVerificationInfo

--------------------------------------------------------------------------------
-- Missing networks

data MissingNetwork = MissingNetwork Text FilePath
  deriving (Generic)

instance FromJSON MissingNetwork
instance ToJSON MissingNetwork

instance Pretty MissingNetwork where
  pretty (MissingNetwork name filepath) =
    pretty name <+> parens (pretty filepath)

--------------------------------------------------------------------------------
-- Altered networks

data AlteredNetwork = AlteredNetwork Text FilePath
  deriving (Generic)

instance FromJSON AlteredNetwork
instance ToJSON AlteredNetwork

instance Pretty AlteredNetwork where
  pretty (AlteredNetwork name filepath) =
    pretty name <+> parens (pretty filepath)

--------------------------------------------------------------------------------
-- Verification status of the specification

data VerificationStatus
  = Verified
  | NetworksMissing (NonEmpty MissingNetwork)
  | NetworksAltered (NonEmpty AlteredNetwork)
  | FailedVerification
  { nameOfProperty :: Text
  , counterexample :: Maybe Text
  }
  deriving (Generic)

instance FromJSON VerificationStatus
instance ToJSON VerificationStatus

instance Pretty VerificationStatus where
  pretty Verified = "Verified"

  pretty (NetworksMissing missingNetworks) =
    "Verification status is currently unknown as the following networks" <+>
    "cannot not be found:" <> line <> line <>
    indent 2 (vsep (fmap pretty missingNetworks)) <> line <> line <>
    "To fix this problem, either move the missing files back to" <+>
    "the" <+> locations <+> "above or use Vehicle to reverify the specification" <+>
    "with the new" <+> locations <> "."
    where
      locations = "location" <> if length missingNetworks == 1 then "" else "s"

  pretty (NetworksAltered alteredNetworks) =
    "Verification status is currently unknown as the following networks" <+>
    "have been altered since verification was last run:" <> line <> line <>
    indent 2 (vsep (fmap pretty alteredNetworks)) <> line <> line <>
    "To fix this problem, use Vehicle to reverify the specification."

  pretty (FailedVerification propertyName counterexample) =
    "Verification was unsuccessful. In particular property" <+>
    squotes (pretty propertyName) <> "could not be verified." <>
    case counterexample of
      Nothing -> ""
      Just e  -> line <> "Counterexample:" <+> pretty e

--------------------------------------------------------------------------------
-- Overall status of the specification

data SpecificationStatus = SpecificationStatus
  { specVersion  :: Version
  , status       :: VerificationStatus
  , networkInfo  :: [NetworkVerificationInfo]
  , originalSpec :: Text
  } deriving (Generic)

instance FromJSON SpecificationStatus
instance ToJSON SpecificationStatus

writeSpecificationStatus :: FilePath -> SpecificationStatus -> IO ()
writeSpecificationStatus file status = ByteString.writeFile file (encode status)

readSpecificationStatus :: FilePath -> IO SpecificationStatus
readSpecificationStatus file = do
  errorOrStatus <- eitherDecode <$> ByteString.readFile file
  case errorOrStatus of
    Right status -> return status
    Left  errorMsg  -> do
      hPutStrLn stderr errorMsg
      exitFailure
