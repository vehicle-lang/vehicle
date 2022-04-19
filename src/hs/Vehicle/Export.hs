
module Vehicle.Export where

import Vehicle.Backend.Prelude
import Vehicle.Verify.VerificationStatus
import Vehicle.Compile
import Vehicle.Backend.Agda (writeAgdaFile, AgdaOptions(..))

data ExportOptions = ExportOptions
  { target             :: ITP
  , proofCacheLocation :: FilePath
  , outputFile         :: Maybe FilePath
  , modulePrefix       :: Maybe String
  } deriving (Show)

export :: LoggingOptions -> ExportOptions -> IO ()
export loggingOptions ExportOptions{..} = do
  proofCache <- readProofCache proofCacheLocation
  let spec = originalSpec proofCache
  let resources = reparseResources (resourceSummaries proofCache)

  case target of
    Agda -> do
      let agdaOptions = AgdaOptions proofCacheLocation outputFile modulePrefix
      agdaCode <- compileToAgda loggingOptions agdaOptions resources spec
      writeAgdaFile outputFile agdaCode
