
module Vehicle.Resource where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.DeepSeq
import Data.Hashable(Hashable(hash))
import Data.ByteString qualified as ByteString
import Data.Map (Map, assocs)
import Data.Text (Text)
import Prettyprinter
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- The different types of resources supported

data ResourceType
  = Network
  | Dataset
  | Parameter
  deriving (Eq, Show, Generic)

instance NFData ResourceType
instance FromJSON ResourceType
instance ToJSON ResourceType

instance Pretty ResourceType where
  pretty = \case
    Network   -> "network"
    Dataset   -> "dataset"
    Parameter -> "parameter"

supportedFileFormats :: ResourceType -> [String]
supportedFileFormats Network = [".onnx"]
supportedFileFormats Dataset = [".idx"]
supportedFileFormats _       = []

--------------------------------------------------------------------------------
-- Locations

type NetworkLocations = Map Text FilePath
type DatasetLocations = Map Text FilePath
type ParameterValues  = Map Text Text

data Resources = Resources
  { networks   :: NetworkLocations
  , datasets   :: DatasetLocations
  , parameters :: ParameterValues
  }

data ResourceSummary = ResourceSummary
  { name     :: Text
  , location :: FilePath
  , fileHash :: Int
  , resType  :: ResourceType
  } deriving (Generic)

instance FromJSON ResourceSummary
instance ToJSON ResourceSummary

--------------------------------------------------------------------------------
-- Hashing

hashResource :: MonadIO m => FilePath -> m Int
hashResource network = do
  contents <- liftIO $ ByteString.readFile network
  return $ hash contents

hashResources :: MonadIO m => Resources -> m [ResourceSummary]
hashResources Resources{..} = do
  networkSummaries <- hashResourceType Network networks
  datasetSummaries <- hashResourceType Dataset datasets
  return $ networkSummaries <> datasetSummaries
  where
  hashResourceType :: MonadIO m
                    => ResourceType
                    -> Map Text FilePath
                    -> m [ResourceSummary]
  hashResourceType resourceType locations =
    forM (assocs locations) $ \(name, location) -> do
      networkHash <- liftIO $ hashResource location
      return $ ResourceSummary
        { name     = name
        , location = location
        , fileHash = networkHash
        , resType  = resourceType
        }
