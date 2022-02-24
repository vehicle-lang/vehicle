module Vehicle.Check
  ( CheckOptions(..)
  , check
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Prelude
import Vehicle.Verify.VerificationStatus
import Vehicle.NeuralNetwork

--------------------------------------------------------------------------------
-- Checking

newtype CheckOptions = CheckOptions
  { databaseFile :: FilePath
  } deriving (Show)

check :: LoggingOptions -> CheckOptions -> IO ()
check loggingOptions checkOptions = do
  status <- checkStatus loggingOptions checkOptions
  let message = layoutAsString (pretty status)
  print message

checkStatus :: LoggingOptions -> CheckOptions -> IO VerificationStatus
checkStatus _loggingOptions CheckOptions{..} = do
  SpecificationStatus{..} <- readSpecificationStatus databaseFile
  case status of
    Verified -> do
      (missingNetworks, alteredNetworks) <- checkNetworkIntegrity networkInfo
      return $ case missingNetworks of
        x : xs -> NetworksMissing (x :| xs)
        []     -> case alteredNetworks of
          x : xs -> NetworksAltered (x :| xs)
          []     -> Verified

    _ -> return status

checkNetworkIntegrity :: [NetworkVerificationInfo]
                      -> IO ([MissingNetwork], [AlteredNetwork])
checkNetworkIntegrity [] = return ([], [])
checkNetworkIntegrity (NetworkVerificationInfo{..} : xs) = do
  (missing, altered) <- checkNetworkIntegrity xs
  -- TODO catch missing file exceptions and use MissingNetwork
  newHash <- hashNetwork location
  if networkHash == newHash then
    return (missing, altered)
  else
    return (missing, AlteredNetwork name location : altered)


  --(\(e :: _) -> return (MissingNetwork name location : missing, altered))
