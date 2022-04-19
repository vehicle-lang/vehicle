module Vehicle.Verify where

import Control.Monad (forM)
import Control.Monad.Trans (MonadIO, liftIO)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Directory (makeAbsolute)

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
  , proofCache       :: Maybe FilePath
  } deriving (Show)

verify :: LoggingOptions -> VerifyOptions -> IO ()
verify loggingOptions VerifyOptions{..} = fromLoggerTIO loggingOptions $ do
  spec <- readInputFile loggingOptions specification
  resources <- convertPathsToAbsolute $
    Resources networkLocations datasetLocations parameterValues

  status <- case verifier of
    Marabou -> do
      marabouSpec <- liftIO $ compileToMarabou loggingOptions resources spec
      liftIO $ Marabou.verifySpec Nothing marabouSpec (networks resources)
    VNNLib  -> do
      liftIO $ hPutStrLn stderr "VNNLib is not currently a valid output target"
      liftIO exitFailure

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

convertPathsToAbsolute :: MonadIO m => Resources -> m Resources
convertPathsToAbsolute Resources{..} = do
  absNetworks <- forM networks (liftIO . makeAbsolute)
  absDatasets <- forM datasets (liftIO . makeAbsolute)
  return $ Resources absNetworks absDatasets parameters