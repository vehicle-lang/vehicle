module Vehicle.Verify
  ( VerifyOptions(..)
  , verify
  ) where

import Control.Monad (forM)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Text.IO (hPutStrLn)
import System.Directory (makeAbsolute, findExecutable, doesFileExist)
import System.Exit (exitFailure)
import System.IO (stderr)

import Vehicle.Compile
import Vehicle.Verify.ProofCache (ProofCache(..), writeProofCache)
import Vehicle.Verify.Core
import Vehicle.Verify.Verifier.Interface
import Vehicle.Verify.Verifier (verifiers)
import Vehicle.Verify.Specification.IO

data VerifyOptions = VerifyOptions
  { specification    :: FilePath
  , properties       :: PropertyNames
  , networkLocations :: NetworkLocations
  , datasetLocations :: DatasetLocations
  , parameterValues  :: ParameterValues
  , verifier         :: VerifierIdentifier
  , verifierLocation :: Maybe VerifierExecutable
  , proofCache       :: Maybe FilePath
  } deriving (Show)

verify :: LoggingOptions -> VerifyOptions -> IO ()
verify loggingOptions VerifyOptions{..} = do
  let verifierImpl = verifiers verifier
  verifierExecutable <- locateVerifierExecutable verifierImpl verifierLocation

  resources <- convertPathsToAbsolute $ Resources networkLocations datasetLocations parameterValues

  uncompiledSpecification <- readSpecification loggingOptions specification

  compiledSpecification <- fromLoggerTIO loggingOptions $ do
    liftIO $ compileToVerifier loggingOptions uncompiledSpecification properties resources verifierImpl

  status <- verifySpecification verifierImpl verifierExecutable networkLocations compiledSpecification

  programOutput loggingOptions $ pretty status

  resourceSummaries <- liftIO $ hashResources resources
  case proofCache of
    Nothing -> return ()
    Just proofCachePath -> writeProofCache proofCachePath $ ProofCache
      { proofCacheVersion  = vehicleVersion
      , originalSpec       = uncompiledSpecification
      , originalProperties = properties
      , status             = status
      , resourceSummaries  = resourceSummaries
      }

locateVerifierExecutable :: MonadIO m => Verifier -> Maybe VerifierExecutable -> m VerifierExecutable
locateVerifierExecutable Verifier{..} = \case
  Just providedLocation -> liftIO $ do
    exists <- doesFileExist providedLocation
    if exists
      then return providedLocation
      else do
        hPutStrLn stderr $ layoutAsText $
          "No" <+> pretty verifierIdentifier <+> "executable found at the provided location" <+>
          squotes (pretty providedLocation) <> "."
        exitFailure

  Nothing -> do
    maybeLocationOnPath <- liftIO $ findExecutable verifierExecutableName
    case maybeLocationOnPath of
      Just locationOnPath -> return locationOnPath
      Nothing -> liftIO $ do
        hPutStrLn stderr $ layoutAsText $
          "Could not locate the executable" <+> quotePretty verifierExecutableName <+>
          "via the PATH environment variable." <> line <>
          "Please either provide it using the `--verifierLocation` command line option" <+>
          "or add it to the PATH environment variable."
        liftIO exitFailure

convertPathsToAbsolute :: MonadIO m => Resources -> m Resources
convertPathsToAbsolute Resources{..} = do
  absNetworks <- forM networks (liftIO . makeAbsolute)
  absDatasets <- forM datasets (liftIO . makeAbsolute)
  return $ Resources absNetworks absDatasets parameters