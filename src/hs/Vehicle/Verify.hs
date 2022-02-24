module Vehicle.Verify where

import Data.Text.IO as TIO (readFile)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Vehicle.NeuralNetwork
import Vehicle.Backend.Prelude
import Vehicle.Backend.Marabou as Marabou (run)
import Vehicle.Compile
import Vehicle.Verify.VerificationStatus
import Control.Monad (forM)

data VerifyOptions = VerifyOptions
  { inputFile      :: FilePath
  , outputFile     :: FilePath
  , verifier       :: Verifier
  , networks       :: [NetworkLocation]
  } deriving (Show)

verify :: LoggingOptions -> VerifyOptions -> IO ()
verify loggingOptions VerifyOptions{..} = do
  spec  <- TIO.readFile inputFile
  status <- case verifier of
    Marabou -> do
      marabouSpec <- compileToMarabou loggingOptions inputFile
      Marabou.run marabouSpec
    VNNLib  -> do
      hPutStrLn stderr "VNNLib is not currently a valid output target"
      exitFailure

  networkInfo <- hashNetworks networks

  writeSpecificationStatus outputFile $ SpecificationStatus
    { version      = vehicleVersion
    , status       = status
    , originalSpec = spec
    , networkInfo  = networkInfo
    }

hashNetworks :: [NetworkLocation] -> IO [NetworkVerificationInfo]
hashNetworks locations = forM locations $ \NetworkLocation{..} -> do
  networkHash <- hashNetwork networkLocation
  return $ NetworkVerificationInfo
    { name        = networkName
    , location    = networkLocation
    , networkHash = networkHash
    }