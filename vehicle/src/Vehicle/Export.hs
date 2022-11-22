
module Vehicle.Export where

import System.Directory (makeAbsolute)

import Vehicle.Backend.Agda (AgdaOptions (..), writeAgdaFile)
import Vehicle.Backend.Prelude
import Vehicle.Compile
import Vehicle.Prelude
import Vehicle.Resource
import Vehicle.Verify.ProofCache

data ExportOptions = ExportOptions
  { target             :: ITP
  , proofCacheLocation :: FilePath
  , outputFile         :: Maybe FilePath
  , moduleName         :: Maybe String
  } deriving (Eq, Show)

export :: VehicleIOSettings -> ExportOptions -> IO ()
export loggingOptions ExportOptions{..} = do
  proofCache <- readProofCache proofCacheLocation
  let spec = originalSpec proofCache
  let properties = originalProperties proofCache
  let resources = reparseResources (resourceSummaries proofCache)

  absoluteProofCacheLocation <- Just <$> makeAbsolute proofCacheLocation
  case target of
    Agda -> do
      let agdaOptions = AgdaOptions absoluteProofCacheLocation outputFile moduleName
      agdaCode <- compileToAgda loggingOptions agdaOptions spec properties resources
      writeAgdaFile outputFile agdaCode
