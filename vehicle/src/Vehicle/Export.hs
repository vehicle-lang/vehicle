module Vehicle.Export where

import Control.Monad.IO.Class (MonadIO (..))
import Vehicle.Backend.Prelude
import Vehicle.Compile
import Vehicle.Prelude.Logging
import Vehicle.Resource
import Vehicle.Verify.Specification (SpecificationCacheIndex (..))
import Vehicle.Verify.Specification.IO

data ExportOptions = ExportOptions
  { target :: ITP,
    verificationCache :: FilePath,
    output :: Maybe FilePath,
    moduleName :: Maybe String
  }
  deriving (Eq, Show)

export :: LoggingSettings -> ExportOptions -> IO ()
export loggingSettings ExportOptions {..} = do
  let cacheIndexFile = specificationCacheIndexFileName verificationCache
  SpecificationCacheIndex {..} <- liftIO $ readSpecificationCacheIndex cacheIndexFile
  let spec = filePath $ specificationSummary resourcesIntegrityInfo
  let resources = reparseResources resourcesIntegrityInfo

  compile loggingSettings $
    CompileOptions
      { target = ITP target,
        specification = spec,
        declarationsToCompile = fmap fst properties,
        networkLocations = networks resources,
        datasetLocations = datasets resources,
        parameterValues = parameters resources,
        output = output,
        moduleName = moduleName,
        verificationCache = Just verificationCache,
        outputAsJSON = False
      }
