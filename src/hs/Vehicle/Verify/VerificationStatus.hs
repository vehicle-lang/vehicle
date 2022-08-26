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

import Vehicle.Resource
import Vehicle.Compile.Prelude

class IsVerified a where
  isVerified :: a -> Bool

--------------------------------------------------------------------------------
-- Verification status of a single property

data SatisfiabilityStatus
  = SAT (Maybe Text)
  | UnSAT
  deriving (Show, Generic)

instance FromJSON SatisfiabilityStatus
instance ToJSON SatisfiabilityStatus
{-
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

-}
--------------------------------------------------------------------------------
-- Verification status of a single property

data PropertyStatus
  = MultiPropertyStatus [PropertyStatus]
  | SinglePropertyStatus Bool (PropertyState SatisfiabilityStatus)
  deriving (Generic)

instance FromJSON PropertyStatus
instance ToJSON PropertyStatus

instance IsVerified PropertyStatus where
  isVerified = \case
    MultiPropertyStatus ps         -> and (fmap isVerified ps)
    SinglePropertyStatus negated s -> do
      let result = case s of
            Trivial    status -> status
            NonTrivial status -> case status of
              SAT _ -> True
              UnSAT -> False

      result `xor` negated

prettyPropertyStatus :: Symbol -> PropertyStatus -> Doc a
prettyPropertyStatus name = \case
  MultiPropertyStatus ps -> do
    let numberedSubproperties =
          zipWith (\(i :: Int) p -> (name <> "!" <> pack (show i), p)) [0..] ps
    let numVerified = pretty (length (filter isVerified ps))
    let num = pretty $ length ps
    let summary = pretty name <> ":" <+> numVerified <> "/" <> num <+> "verified"
    let results = indent 2 $ vsep (fmap (uncurry prettyPropertyStatus) numberedSubproperties)
    summary <> line <> results

  SinglePropertyStatus negated s -> do
    let (verified, evidenceText) = case s of
          Trivial    status -> (status `xor` negated, " (trivial)")
          NonTrivial status -> case status of
            UnSAT       -> (negated, "")
            SAT witness -> do
              let witnessText = if negated then "Counter-example" else "Witness"
              let formatWitness e = line <> indent 2 (witnessText <> ":" <+> pretty e)
              let witnessDoc = maybe "" formatWitness witness
              (not negated, witnessDoc)

    prettyNameAndStatus name verified <> evidenceText

prettyNameAndStatus :: Text -> Bool -> Doc a
prettyNameAndStatus name verified = do
  let (colour, symbol) = if verified then (Green, "🗸") else (Red, "✗")
  pretty (setTextColour colour symbol) <+> pretty name

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
    <> indent 2 (vsep (fmap (uncurry prettyPropertyStatus) (Map.toList properties)))

--------------------------------------------------------------------------------
-- Overall status of the specification

data ProofCache = ProofCache
  { proofCacheVersion  :: Version
  , status             :: SpecificationStatus
  , resourceSummaries  :: [ResourceSummary]
  , originalSpec       :: Specification
  , originalProperties :: Properties
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