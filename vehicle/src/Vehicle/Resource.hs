
module Vehicle.Resource where

import Control.DeepSeq
import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as ByteString
import Data.Hashable (Hashable (hash))
import Data.Map (Map, assocs, singleton)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- The different types of resources supported

data Resource
  = Network
  | Dataset
  | Parameter
  | InferableParameter
  deriving (Eq, Show, Generic)

instance NFData Resource
instance FromJSON Resource
instance ToJSON Resource

instance Pretty Resource where
  pretty = \case
    Network            -> "network"
    Dataset            -> "dataset"
    Parameter          -> "parameter"
    InferableParameter -> "inferable parameter"

supportedFileFormats :: Resource -> [String]
supportedFileFormats Network = [".onnx"]
supportedFileFormats Dataset = [".idx"]
supportedFileFormats _       = []

--------------------------------------------------------------------------------
-- Resource locations

type NetworkLocations = Map Text FilePath
type DatasetLocations = Map Text FilePath
type ParameterValues  = Map Text String

data Resources = Resources
  { networks   :: NetworkLocations
  , datasets   :: DatasetLocations
  , parameters :: ParameterValues
  }

instance Semigroup Resources where
  r1 <> r2 = Resources
    (networks   r1 <> networks   r2)
    (datasets   r1 <> datasets   r2)
    (parameters r1 <> parameters r2)

instance Monoid Resources where
  mempty = Resources mempty mempty mempty

--------------------------------------------------------------------------------
-- Resource summaries

data ResourceSummary = ResourceSummary
  { name     :: Text
  , value    :: String
  , fileHash :: Int
  , resType  :: Resource
  } deriving (Generic)

instance FromJSON ResourceSummary
instance ToJSON ResourceSummary

--------------------------------------------------------------------------------
-- Hashing

hashResource :: MonadIO m => Resource -> String -> m Int
hashResource Network           filepath = liftIO $ hash <$> ByteString.readFile filepath
hashResource Dataset           filepath = liftIO $ hash <$> ByteString.readFile filepath
hashResource Parameter         value    = return $ hash value
hashResource InferableParameter _        =
  developerError "Should not be hashing implicit parameters"

hashResources :: MonadIO m => Resources -> m [ResourceSummary]
hashResources Resources{..} = do
  networkSummaries   <- hashResourceType Network   networks
  datasetSummaries   <- hashResourceType Dataset   datasets
  parameterSummaries <- hashResourceType Parameter parameters
  return $ networkSummaries <> datasetSummaries <> parameterSummaries
  where
  hashResourceType :: MonadIO m
                   => Resource
                   -> Map Text String
                   -> m [ResourceSummary]
  hashResourceType resourceType values =
    forM (assocs values) $ \(name, value) -> do
      networkHash <- liftIO $ hashResource resourceType value
      return $ ResourceSummary
        { name     = name
        , value    = value
        , fileHash = networkHash
        , resType  = resourceType
        }

reparseResources :: [ResourceSummary] -> Resources
reparseResources []       = mempty
reparseResources (x : xs) = r <> reparseResources xs
  where
    v = singleton (name x) (value x)
    r = case resType x of
      Network           -> Resources v mempty mempty
      Dataset           -> Resources mempty v mempty
      Parameter         -> Resources mempty mempty v
      InferableParameter -> developerError "Should not be reparsing implicit parameters"


--------------------------------------------------------------------------------
-- Others

warnIfUnusedResources :: MonadLogger m
                      => Resource
                      -> Set Name
                      -> Set Name
                      -> m ()
warnIfUnusedResources resourceType given found = do
  when (null found) $
    logDebug MinDetail $ "No" <+> pretty resourceType <> "s found in program"

  let unusedParams = given `Set.difference` found
  when (Set.size unusedParams > 0) $
    logWarning $ "the following" <+> pretty resourceType <> "s were provided" <+>
                 "but not used by the specification:" <+> prettySet unusedParams
