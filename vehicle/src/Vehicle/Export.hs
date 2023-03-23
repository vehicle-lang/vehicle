module Vehicle.Export where

import Vehicle.Backend.Prelude
import Vehicle.Compile
import Vehicle.Prelude
import Vehicle.Resource
import Vehicle.Verify.ProofCache

data ExportOptions = ExportOptions
  { target :: ITP,
    proofCacheLocation :: FilePath,
    outputFile :: Maybe FilePath,
    moduleName :: Maybe String
  }
  deriving (Eq, Show)

export :: LoggingSettings -> ExportOptions -> IO ()
export loggingSettings ExportOptions {..} = do
  proofCache <- readProofCache proofCacheLocation
  let spec = originalSpec proofCache
  let properties = originalProperties proofCache
  let resources = reparseResources (resourceSummaries proofCache)

  compile loggingSettings $
    CompileOptions
      { task = CompileToITP target,
        specification = spec,
        declarationsToCompile = properties,
        networkLocations = networks resources,
        datasetLocations = datasets resources,
        parameterValues = parameters resources,
        outputFile = outputFile,
        moduleName = moduleName,
        proofCache = Just proofCacheLocation,
        noStdlib = False
      }
