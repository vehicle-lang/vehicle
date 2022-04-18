
module Vehicle.Resource where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.DeepSeq
import Data.Hashable(Hashable(hash))
import Data.ByteString qualified as ByteString
import Data.Map (Map, assocs)
import Data.Text (Text)
import Data.Set (Set)
import Data.Set qualified as Set
import Prettyprinter
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Vehicle.Prelude

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


--------------------------------------------------------------------------------
-- Others

warnIfUnusedResources :: MonadLogger m
                      => ResourceType
                      -> Set Symbol
                      -> Set Symbol
                      -> m ()
warnIfUnusedResources resourceType given found = do
  when (null found) $
    logDebug MinDetail $ "No" <+> pretty resourceType <> "s found in program"

  let unusedParams = given `Set.difference` found
  when (Set.size unusedParams > 0) $
    logWarning $ "the following" <+> pretty resourceType <> "s were provided" <+>
                 "but not used by the specification:" <+> prettySet unusedParams