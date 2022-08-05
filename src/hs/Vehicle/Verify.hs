module Vehicle.Verify where

import Control.Monad (forM)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Text.IO (hPutStrLn)
import System.Directory (makeAbsolute, findExecutable, doesFileExist)
import System.Exit (exitFailure)
import System.IO (stderr)

import Vehicle.Backend.Prelude
import Vehicle.Backend.Marabou as Marabou (verifySpec)
import Vehicle.Compile
import Vehicle.Verify.VerificationStatus

data VerifyOptions = VerifyOptions
  { specification    :: FilePath
  , networkLocations :: NetworkLocations
  , datasetLocations :: DatasetLocations
  , parameterValues  :: ParameterValues
  , verifier         :: Verifier
  , verifierLocation :: Maybe FilePath
  , proofCache       :: Maybe FilePath
  } deriving (Show)

verify :: LoggingOptions -> VerifyOptions -> IO ()
verify loggingOptions VerifyOptions{..} = fromLoggerTIO loggingOptions $ do
  verifierExectuable <- locateVerifier verifier verifierLocation
  spec <- readInputFile loggingOptions specification
  resources <- convertPathsToAbsolute $
    Resources networkLocations datasetLocations parameterValues

  status <- case verifier of
    Marabou -> do
      marabouSpec <- liftIO $ compileToMarabou loggingOptions resources spec
      liftIO $ Marabou.verifySpec verifierExectuable marabouSpec (networks resources)

  programOutput loggingOptions $ pretty status

  resourceSummaries <- liftIO $ hashResources resources
  case proofCache of
    Nothing -> return ()
    Just proofCachePath -> writeProofCache proofCachePath $ ProofCache
      { proofCacheVersion = vehicleVersion
      , status            = status
      , originalSpec      = spec
      , resourceSummaries = resourceSummaries
      }

locateVerifier :: MonadIO m => Verifier -> Maybe FilePath -> m FilePath
locateVerifier verifier = \case
  Just providedLocation -> liftIO $ do
    exists <- doesFileExist providedLocation
    if exists
      then return providedLocation
      else do
        hPutStrLn stderr $ layoutAsText $
          "No" <+> pretty verifier <+> "executable found at the provided location" <+>
          squotes (pretty providedLocation) <> "."
        exitFailure

  Nothing -> do
    let verifierName = verifierExecutableName verifier
    maybeLocationOnPath <- liftIO $ findExecutable verifierName
    case maybeLocationOnPath of
      Just locationOnPath -> return locationOnPath
      Nothing -> liftIO $ do
        hPutStrLn stderr $ layoutAsText $
          "Could not locate the executable" <+> squotes (pretty verifierName) <+>
          "via the PATH environment variable." <> line <>
          "Please either provide it using the `--verifierLocation` command line option" <+>
          "or add it to the PATH environment variable."
        liftIO exitFailure

convertPathsToAbsolute :: MonadIO m => Resources -> m Resources
convertPathsToAbsolute Resources{..} = do
  absNetworks <- forM networks (liftIO . makeAbsolute)
  absDatasets <- forM datasets (liftIO . makeAbsolute)
  return $ Resources absNetworks absDatasets parameters