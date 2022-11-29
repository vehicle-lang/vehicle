
module Vehicle.Export where

import System.Directory (makeAbsolute)

import Vehicle.Backend.Agda (AgdaOptions (..))
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

export :: LoggingSettings -> ExportOptions -> IO ()
export loggingSettings ExportOptions{..} = do
  proofCache <- readProofCache proofCacheLocation
  let spec = originalSpec proofCache
  let properties = originalProperties proofCache
  let _resources = reparseResources (resourceSummaries proofCache)

  absoluteProofCacheLocation <- Just <$> makeAbsolute proofCacheLocation
  case target of
    Agda -> runCompileMonad loggingSettings $ do
      let agdaOptions = AgdaOptions absoluteProofCacheLocation outputFile moduleName
      typingCheckingResult <-  typeCheckProg spec properties
      _ <- compileToAgda agdaOptions typingCheckingResult outputFile
      return ()
