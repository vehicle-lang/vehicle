{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Resource where

import Control.Exception (IOException, catch)
import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as ByteString
import Data.Hashable (Hashable (hash))
import Data.Map (Map, assocs)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter
import Vehicle.Prelude
import Vehicle.Syntax.AST

--------------------------------------------------------------------------------
-- The different types of resources supported

data ExternalResource
  = Network
  | Dataset
  | Parameter
  deriving (Eq, Show, Generic)

instance Pretty ExternalResource where
  pretty = \case
    Network -> "network"
    Dataset -> "dataset"
    Parameter -> "parameter"

supportedFileFormats :: ExternalResource -> [String]
supportedFileFormats Network = [".onnx"]
supportedFileFormats Dataset = [".idx"]
supportedFileFormats Parameter = []

--------------------------------------------------------------------------------
-- Resource locations

type SpecificationLocation = FilePath

type NetworkLocations = Map Name FilePath

type DatasetLocations = Map Name FilePath

type ParameterValues = Map Name String

data Resources = Resources
  { specification :: SpecificationLocation,
    networks :: NetworkLocations,
    datasets :: DatasetLocations,
    parameters :: ParameterValues
  }

--------------------------------------------------------------------------------
-- Resource summaries

data ResourceIntegrityInfo = ResourceIntegrityInfo
  { name :: Text,
    filePath :: FilePath,
    fileHash :: Int
  }
  deriving (Generic)

instance FromJSON ResourceIntegrityInfo

instance ToJSON ResourceIntegrityInfo

data ResourcesIntegrityInfo = ResourcesIntegrityInfo
  { specificationSummary :: ResourceIntegrityInfo,
    networkSummaries :: [ResourceIntegrityInfo],
    datasetSummaries :: [ResourceIntegrityInfo],
    parameterSummaries :: ParameterValues
  }
  deriving (Generic)

instance FromJSON ResourcesIntegrityInfo

instance ToJSON ResourcesIntegrityInfo

--------------------------------------------------------------------------------
-- Hashing

hashFileContents :: (MonadIO m) => FilePath -> m Int
hashFileContents filePath = do
  fileContents <- liftIO $ ByteString.readFile filePath
  return $ hash fileContents

generateResourceIntegrityInfo :: (MonadIO m) => (Name, FilePath) -> m ResourceIntegrityInfo
generateResourceIntegrityInfo (name, filePath) = do
  fileHash <-
    liftIO $
      catch @IOException
        (hashFileContents filePath)
        ( \e ->
            fatalError $
              "Error occured while reading"
                <+> quotePretty filePath
                <> ":"
                <> line
                <> indent 2 (pretty (show e))
        )

  return $
    ResourceIntegrityInfo
      { name = name,
        filePath = filePath,
        fileHash = fileHash
      }

generateResourcesIntegrityInfo :: (MonadIO m) => Resources -> m ResourcesIntegrityInfo
generateResourcesIntegrityInfo Resources {..} = do
  specificationSummary <- generateResourceIntegrityInfo ("specification", specification)
  networkSummaries <- forM (assocs networks) generateResourceIntegrityInfo
  datasetSummaries <- forM (assocs datasets) generateResourceIntegrityInfo
  return $
    ResourcesIntegrityInfo
      { specificationSummary = specificationSummary,
        networkSummaries = networkSummaries,
        datasetSummaries = datasetSummaries,
        parameterSummaries = parameters
      }

data ResourceIntegrityStatus
  = Unchanged
  | Altered
  | Missing

checkResourceIntegrity :: (MonadIO m) => ResourceIntegrityInfo -> m ResourceIntegrityStatus
checkResourceIntegrity ResourceIntegrityInfo {..} = do
  maybeNewHash <-
    liftIO $
      catch @IOException
        (Just <$> hashFileContents filePath)
        (const $ return Nothing)

  return $ case maybeNewHash of
    Nothing -> Missing
    Just newFileHash
      | fileHash /= newFileHash -> Altered
      | otherwise -> Unchanged

checkResourcesIntegrity ::
  (MonadIO m) =>
  [ResourceIntegrityInfo] ->
  m ([ResourceIntegrityInfo], [ResourceIntegrityInfo])
checkResourcesIntegrity = \case
  [] -> return ([], [])
  (r : rs) -> do
    (missing, altered) <- checkResourcesIntegrity rs
    resourceStatus <- liftIO (checkResourceIntegrity r)
    return $ case resourceStatus of
      Unchanged -> (missing, altered)
      Altered -> (missing, r : altered)
      Missing -> (r : missing, altered)

checkIntegrityOfResources ::
  (MonadIO m) =>
  ResourcesIntegrityInfo ->
  m ([ResourceIntegrityInfo], [ResourceIntegrityInfo])
checkIntegrityOfResources ResourcesIntegrityInfo {..} =
  checkResourcesIntegrity $ specificationSummary : networkSummaries <> datasetSummaries

reparseResources :: ResourcesIntegrityInfo -> Resources
reparseResources ResourcesIntegrityInfo {..} = do
  Resources
    { specification = filePath specificationSummary,
      networks = reparseResourceType networkSummaries,
      datasets = reparseResourceType datasetSummaries,
      parameters = parameterSummaries
    }

reparseResourceType :: [ResourceIntegrityInfo] -> Map Name String
reparseResourceType = foldr (\info -> Map.insert (name info) (filePath info)) mempty

--------------------------------------------------------------------------------
-- Others

warnIfUnusedResources ::
  (MonadLogger m, HasName ident Name) =>
  ExternalResource ->
  Map Name a ->
  Map ident b ->
  m ()
warnIfUnusedResources resourceType given found = do
  when (null found) $
    logDebug MinDetail $
      "No" <+> pretty resourceType <> "s found in program"

  let givenNames = Map.keysSet given
  let foundNames = Set.map nameOf $ Map.keysSet found
  let unusedParams = givenNames `Set.difference` foundNames
  when (Set.size unusedParams > 0) $
    logWarning $
      "the following"
        <+> pretty resourceType
        <> "s were provided"
        <+> "but not used by the specification:"
        <+> prettySet unusedParams
