module Vehicle.Verify.VerificationStatus where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text (Text, pack)
import Data.ByteString.Lazy qualified as ByteString
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Version (Version)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Console.ANSI (Color(..))

import Vehicle.Prelude
import Vehicle.Resource

class IsVerified a where
  isVerified :: a -> Bool

--------------------------------------------------------------------------------
-- Verification status of a single property

data SinglePropertyStatus
  -- | Witness
  = Verified (Maybe Text)
  -- | Counter-example
  | Failed (Maybe Text)
  deriving (Show, Generic)

instance FromJSON SinglePropertyStatus
instance ToJSON SinglePropertyStatus

instance IsVerified SinglePropertyStatus where
  isVerified Verified{} = True
  isVerified Failed{}   = False

-- | Negates the status of the property under the assumption that it
-- represents the status of an existential satisfaction problem.
negateStatus :: SinglePropertyStatus -> SinglePropertyStatus
negateStatus (Verified witness) = Failed witness
negateStatus (Failed   _)       = Verified Nothing

exampleOf :: SinglePropertyStatus -> Maybe Text
exampleOf (Verified e) = e
exampleOf (Failed   e) = e

prettySinglePropertyStatus :: (Symbol, SinglePropertyStatus) -> Doc a
prettySinglePropertyStatus (name, status) =
  pretty symbol <+> pretty name <> example
  where
  (symbol, exampleText) = if isVerified status
    then (setTextColour Green "🗸", "Witness")
    else (setTextColour Red   "✗",  "Counter-example")

  example = case exampleOf status of
    Nothing -> ""
    Just e  -> line <> indent 2 (exampleText <> ":" <+> pretty e)

--------------------------------------------------------------------------------
-- Verification status of a single property

data PropertyStatus
  = MultiPropertyStatus [PropertyStatus]
  | SinglePropertyStatus SinglePropertyStatus
  deriving (Show, Generic)

instance FromJSON PropertyStatus
instance ToJSON PropertyStatus

instance IsVerified PropertyStatus where
  isVerified (MultiPropertyStatus ps) = and (fmap isVerified ps)
  isVerified (SinglePropertyStatus s) = isVerified s

prettyPropertyStatus :: (Symbol, PropertyStatus) -> Doc a
prettyPropertyStatus (name, MultiPropertyStatus ps) =
  let numberedSubproperties =
        zipWith (\(i :: Int) p -> (name <> "!" <> pack (show i), p)) [0..] ps in
  let numVerified = pretty (length (filter isVerified ps)) in
  let num = pretty $ length ps in
  let summary = pretty name <> ":" <+> numVerified <> "/" <> num <+> "verified" in
  let results = indent 2 $ vsep (fmap prettyPropertyStatus numberedSubproperties) in
  summary <> line <> results
prettyPropertyStatus (name, SinglePropertyStatus s) =
  prettySinglePropertyStatus (name, s)

--------------------------------------------------------------------------------
-- Verification status of the specification

newtype SpecificationStatus = SpecificationStatus (Map Text PropertyStatus)
  deriving (Generic)

instance FromJSON SpecificationStatus
instance ToJSON SpecificationStatus

instance IsVerified SpecificationStatus where
  isVerified (SpecificationStatus properties) =
    and (fmap isVerified (Map.elems properties))

instance Pretty SpecificationStatus where
  pretty spec@(SpecificationStatus properties) =
    (if isVerified spec
      then "Result: verified"
      else "Result: unverified")
    <> line
    <> indent 2 (vsep (fmap prettyPropertyStatus (Map.toList properties)))

--------------------------------------------------------------------------------
-- Overall status of the specification

data ProofCache = ProofCache
  { proofCacheVersion :: Version
  , status            :: SpecificationStatus
  , resourceSummaries :: [ResourceSummary]
  , originalSpec      :: Text
  } deriving (Generic)

instance FromJSON ProofCache
instance ToJSON ProofCache

writeProofCache :: MonadIO m => FilePath -> ProofCache -> m ()
writeProofCache file status = liftIO $ ByteString.writeFile file (encodePretty status)

readProofCache :: FilePath -> IO ProofCache
readProofCache file = do
  errorOrStatus <- eitherDecode <$> ByteString.readFile file
  case errorOrStatus of
    Right status -> return status
    Left  errorMsg  -> do
      hPutStrLn stderr errorMsg
      exitFailure