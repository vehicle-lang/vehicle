module Vehicle.Verify
  ( VerifyOptions (..),
    VerifierID,
    verify,
  )
where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Text.IO (hPutStrLn)
import System.Directory (doesFileExist, findExecutable)
import System.Exit (exitFailure)
import System.IO (stderr)
import Vehicle.Prelude
import Vehicle.Verify.Core
import Vehicle.Verify.ProofCache (ProofCache (..), writeProofCache)
import Vehicle.Verify.Specification (VerificationPlan (VerificationPlan), specificationPropertyNames)
import Vehicle.Verify.Specification.IO
import Vehicle.Verify.Verifier (verifiers)

data VerifyOptions = VerifyOptions
  { queryFolder :: FilePath,
    verifierID :: VerifierID,
    verifierLocation :: Maybe VerifierExecutable,
    proofCache :: Maybe FilePath
  }
  deriving (Eq, Show)

verify :: LoggingSettings -> VerifyOptions -> IO ()
verify _loggingSettings VerifyOptions {..} = do
  let verifierImpl = verifiers verifierID
  verifierExecutable <- locateVerifierExecutable verifierImpl verifierLocation

  let verificationPlanFile = verificationPlanFileName queryFolder
  VerificationPlan specificationPlan resourceIntegrity <- readVerificationPlan verificationPlanFile
  status <- verifySpecification queryFolder verifierImpl verifierExecutable specificationPlan

  programOutput $ pretty status
  case proofCache of
    Nothing -> return ()
    Just proofCachePath ->
      writeProofCache proofCachePath $
        ProofCache
          { proofCacheVersion = preciseVehicleVersion,
            resourcesIntegrityInfo = resourceIntegrity,
            originalProperties = specificationPropertyNames specificationPlan,
            status = status
          }

-- | Tries to locate the executable for the verifier at the provided
-- location and falls back to the PATH variable if none provided. If not
-- found then the program will error.
locateVerifierExecutable ::
  (MonadIO m) =>
  Verifier ->
  Maybe VerifierExecutable ->
  m VerifierExecutable
locateVerifierExecutable Verifier {..} = \case
  Just providedLocation -> liftIO $ do
    exists <- doesFileExist providedLocation
    if exists
      then return providedLocation
      else do
        hPutStrLn stderr $
          layoutAsText $
            "No"
              <+> pretty verifierIdentifier
              <+> "executable found"
              <+> "at the provided location"
              <+> quotePretty providedLocation
              <> "."
        exitFailure
  Nothing -> do
    maybeLocationOnPath <- liftIO $ findExecutable verifierExecutableName
    case maybeLocationOnPath of
      Just locationOnPath -> return locationOnPath
      Nothing -> liftIO $ do
        hPutStrLn stderr $
          layoutAsText $
            "Could not locate the executable"
              <+> quotePretty verifierExecutableName
              <+> "via the PATH environment variable."
              <> line
              <> "Please either provide it using the `--verifierLocation` command line option"
                <+> "or add it to the PATH environment variable."
        liftIO exitFailure
