
module Vehicle.Export where

import Vehicle.Backend.Prelude
import Vehicle.Verify.VerificationStatus
import Vehicle.Compile
import Vehicle.Backend.Agda (writeAgdaFile, AgdaOptions(..))
import System.Directory (makeAbsolute)

data ExportOptions = ExportOptions
  { target             :: ITP
  , proofCacheLocation :: FilePath
  , outputFile         :: Maybe FilePath
  , moduleName         :: Maybe String
  } deriving (Show)

export :: LoggingOptions -> ExportOptions -> IO ()
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
