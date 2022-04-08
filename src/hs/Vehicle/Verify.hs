module Vehicle.Verify where

import Control.Monad (forM)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Map ( Map )
import Data.Text (Text)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import System.Directory (makeAbsolute)

import Vehicle.Backend.Prelude
import Vehicle.Backend.Marabou as Marabou (verifySpec)
import Vehicle.Compile
import Vehicle.Verify.VerificationStatus

data VerifyOptions = VerifyOptions
  { verifier       :: Verifier
  , inputFile      :: FilePath
  , networks       :: Map Text FilePath
  , datasets       :: Map Text FilePath
  , proofCache     :: Maybe FilePath
  } deriving (Show)

verify :: LoggingOptions -> VerifyOptions -> IO ()
verify loggingOptions VerifyOptions{..} = fromLoggerTIO loggingOptions $ do
  spec <- readInputFile loggingOptions inputFile
  let resourceLocations = collateResourceLocations networks datasets
  absoluteResourceLocations <- convertPathsToAbsolute resourceLocations

  status <- case verifier of
    Marabou -> do
      marabouSpec <- liftIO $ compileToMarabou loggingOptions spec absoluteResourceLocations
      liftIO $ Marabou.verifySpec Nothing marabouSpec resourceLocations
    VNNLib  -> do
      liftIO $ hPutStrLn stderr "VNNLib is not currently a valid output target"
      liftIO exitFailure

  programOutput loggingOptions $ pretty status

  resources <- liftIO $ hashResources resourceLocations
  case proofCache of
    Nothing -> return ()
    Just proofCachePath -> writeProofCache proofCachePath $ ProofCache
      { specVersion  = vehicleVersion
      , status       = status
      , originalSpec = spec
      , resources    = resources
      }

convertPathsToAbsolute :: MonadIO m => ResourceLocations -> m ResourceLocations
convertPathsToAbsolute resources =
  forM resources $ \(t, v) -> do
    v' <- liftIO $ makeAbsolute v
    return (t, v')