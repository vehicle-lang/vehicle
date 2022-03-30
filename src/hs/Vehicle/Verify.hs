module Vehicle.Verify where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map ( assocs )
import Data.Text.IO as TIO (readFile)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Directory (makeAbsolute)

import Vehicle.NeuralNetwork
import Vehicle.Backend.Prelude
import Vehicle.Backend.Marabou as Marabou (verifySpec)
import Vehicle.Compile
import Vehicle.Verify.VerificationStatus
import Control.Monad (forM)

data VerifyOptions = VerifyOptions
  { verifier       :: Verifier
  , inputFile      :: FilePath
  , networks       :: NetworkLocations
  , proofCache     :: Maybe FilePath
  } deriving (Show)

verify :: LoggingOptions -> VerifyOptions -> IO ()
verify loggingOptions VerifyOptions{..} = fromLoggerTIO loggingOptions $ do
  spec  <- liftIO $ TIO.readFile inputFile
  networks' <- convertResourcesPathsToAbsolute networks

  status <- case verifier of
    Marabou -> do
      marabouSpec <- liftIO $ compileToMarabou loggingOptions inputFile
      liftIO $ Marabou.verifySpec Nothing marabouSpec networks'
    VNNLib  -> do
      liftIO $ hPutStrLn stderr "VNNLib is not currently a valid output target"
      liftIO exitFailure

  programOutput loggingOptions $ pretty status

  networkInfo <- liftIO $ hashNetworks networks'
  case proofCache of
    Nothing -> return ()
    Just proofCachePath -> writeProofCache proofCachePath $ ProofCache
      { specVersion  = vehicleVersion
      , status       = status
      , originalSpec = spec
      , networkInfo  = networkInfo
      }

hashNetworks :: MonadIO m => NetworkLocations -> m [NetworkVerificationInfo]
hashNetworks locations = forM (assocs locations) $ \(networkName, networkLocation) -> do
  networkHash <- liftIO $ hashNetwork networkLocation
  return $ NetworkVerificationInfo
    { name        = networkName
    , location    = networkLocation
    , networkHash = networkHash
    }

convertResourcesPathsToAbsolute :: MonadIO m => NetworkLocations -> m NetworkLocations
convertResourcesPathsToAbsolute locations = forM locations $
  \v -> liftIO $ makeAbsolute v