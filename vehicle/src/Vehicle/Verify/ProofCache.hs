module Vehicle.Verify.ProofCache where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as ByteString
import Data.Version (Version)
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import System.FilePath (dropExtension)
import System.IO (hPutStrLn, stderr)
import Vehicle.Compile.Prelude
import Vehicle.Verify.Specification.Status (SpecificationStatus)

--------------------------------------------------------------------------------
-- Overall status of the specification

data ProofCache = ProofCache
  { proofCacheVersion :: Version,
    status :: SpecificationStatus,
    resourceSummaries :: [ResourceSummary],
    originalSpec :: FilePath,
    originalSpecHash :: Int,
    originalProperties :: PropertyNames
  }
  deriving (Generic)

instance FromJSON ProofCache

instance ToJSON ProofCache

writeProofCache :: MonadIO m => FilePath -> ProofCache -> m ()
writeProofCache file status = do
  let filepath = dropExtension file <> vehicleProofCacheFileExtension
  liftIO $ ByteString.writeFile filepath (encodePretty status)

readProofCache :: FilePath -> IO ProofCache
readProofCache file = do
  errorOrStatus <- eitherDecode <$> ByteString.readFile file
  case errorOrStatus of
    Right status -> return status
    Left errorMsg -> do
      hPutStrLn stderr errorMsg
      exitFailure
