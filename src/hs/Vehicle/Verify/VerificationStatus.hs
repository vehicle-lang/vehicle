module Vehicle.Verify.VerificationStatus where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy qualified as ByteString
import Data.Map
import Data.Version (Version)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Console.ANSI (Color(..))
import System.Directory (makeAbsolute)

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
-- Verification status of a single property

data PropertyStatus
  = Verified { witness        :: Maybe Text }
  | Failed   { counterexample :: Maybe Text }
  deriving (Show, Generic)

instance FromJSON PropertyStatus
instance ToJSON PropertyStatus

isVerified :: PropertyStatus -> Bool
isVerified (Verified _) = True
isVerified _            = False

-- | Negates the status of the property under the assumption that it
-- represents the status of an existential satisfaction problem.
negateStatus :: PropertyStatus -> PropertyStatus
negateStatus (Verified witness) = Failed witness
negateStatus (Failed   _)       = Verified Nothing

exampleOf :: PropertyStatus -> Maybe Text
exampleOf (Verified e) = e
exampleOf (Failed   e) = e

prettyPropertyStatus :: (Text, PropertyStatus) -> Doc a
prettyPropertyStatus (name, status) =
  pretty symbol <+> pretty name <> example
  where
  (symbol, exampleText) = if isVerified status
    then (setTextColour Green "🗸", "Witness")
    else (setTextColour Red   "✗",  "Counter-example")

  example = case exampleOf status of
    Nothing -> ""
    Just e  -> line <> indent 2 (exampleText <> ":" <+> pretty e)


--------------------------------------------------------------------------------
-- Verification status of the specification

newtype SpecificationStatus = SpecificationStatus (Map Text PropertyStatus)
  deriving (Generic)

instance FromJSON SpecificationStatus
instance ToJSON SpecificationStatus

isSpecVerified :: SpecificationStatus -> Bool
isSpecVerified (SpecificationStatus properties) =
  and (fmap isVerified (elems properties))

instance Pretty SpecificationStatus where
  pretty spec@(SpecificationStatus properties) =
    (if isSpecVerified spec
      then "Result: verified"
      else "Result: unverified") <> line <>
    indent 2 (vsep (fmap prettyPropertyStatus (toList properties)))

--------------------------------------------------------------------------------
-- Overall status of the specification

data ProofCache = ProofCache
  { specVersion  :: Version
  , status       :: SpecificationStatus
  , networkInfo  :: [NetworkVerificationInfo]
  , originalSpec :: Text
  } deriving (Generic)

instance FromJSON ProofCache
instance ToJSON ProofCache

writeProofCache :: MonadIO m => FilePath -> ProofCache -> m ()
writeProofCache file status = liftIO $ ByteString.writeFile file (encode status)

readProofCache :: FilePath -> IO ProofCache
readProofCache file = do
  errorOrStatus <- eitherDecode <$> ByteString.readFile file
  case errorOrStatus of
    Right status -> return status
    Left  errorMsg  -> do
      hPutStrLn stderr errorMsg
      exitFailure

getProofCacheLocation :: MonadIO m => LoggingOptions -> Maybe FilePath -> m FilePath
getProofCacheLocation _              (Just location) = liftIO $ makeAbsolute location
getProofCacheLocation loggingOptions Nothing         =
  fatalError loggingOptions "You must provide a value for `proofCache` when compiling to Agda"
