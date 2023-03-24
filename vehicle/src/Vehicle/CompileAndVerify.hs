module Vehicle.CompileAndVerify
  ( CompileAndVerifyOptions (..),
    compileAndVerify,
  )
where

import System.IO.Temp (withSystemTempDirectory)
import Vehicle.Backend.Prelude (Target (..))
import Vehicle.Compile (CompileOptions (..), compile)
import Vehicle.Prelude
import Vehicle.Resource
import Vehicle.Verify (VerifyOptions (..), verify)
import Vehicle.Verify.Core
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier (verifiers)

data CompileAndVerifyOptions = CompileAndVerifyOptions
  { specification :: FilePath,
    properties :: PropertyNames,
    networkLocations :: NetworkLocations,
    datasetLocations :: DatasetLocations,
    parameterValues :: ParameterValues,
    verifierID :: VerifierID,
    verifierLocation :: Maybe VerifierExecutable,
    proofCache :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | Compiles the specification to a temporary directory and then tries to verify it.
compileAndVerify :: LoggingSettings -> CompileAndVerifyOptions -> IO ()
compileAndVerify loggingSettings CompileAndVerifyOptions {..} = do
  let target = VerifierQueries $ verifierQueryFormat $ verifiers verifierID

  withSystemTempDirectory "specification" $ \tempDir -> do
    compile loggingSettings $
      CompileOptions
        { target = target,
          specification = specification,
          declarationsToCompile = properties,
          outputFile = Just tempDir,
          moduleName = Nothing,
          proofCache = Nothing,
          ..
        }

    verify loggingSettings $
      VerifyOptions
        { verificationPlan = verificationPlanFileName tempDir,
          verifierID = verifierID,
          verifierLocation = verifierLocation,
          proofCache = proofCache
        }
