
module Vehicle.Resource.Core where

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
  deriving (Eq, Show, Generic)

instance NFData ResourceType
instance FromJSON ResourceType
instance ToJSON ResourceType

instance Pretty ResourceType where
  pretty = \case
    Network  -> "network"
    Dataset  -> "dataset"

supportedFileFormats :: ResourceType -> [String]
supportedFileFormats Network = [".onnx"]
supportedFileFormats Dataset = [".idx"]

--------------------------------------------------------------------------------
-- Locations

type ResourceLocations = Map Text (ResourceType, FilePath)

collateResourceLocations :: Map Text FilePath
                         -> Map Text FilePath
                         -> ResourceLocations
collateResourceLocations networks datasets =
  fmap (Network,) networks <> fmap (Dataset,) datasets

data Resource = Resource
  { name     :: Text
  , location :: FilePath
  , fileHash :: Int
  , resType  :: ResourceType
  } deriving (Generic)

instance FromJSON Resource
instance ToJSON Resource

--------------------------------------------------------------------------------
-- Hashing

hashResource :: MonadIO m => FilePath -> m Int
hashResource network = do
  contents <- liftIO $ ByteString.readFile network
  return $ hash contents

hashResources :: MonadIO m => ResourceLocations -> m [Resource]
hashResources locations = forM (assocs locations) $
  \(name, (resourceType, location)) -> do
    networkHash <- liftIO $ hashResource location
    return $ Resource
      { name     = name
      , location = location
      , fileHash = networkHash
      , resType  = resourceType
      }