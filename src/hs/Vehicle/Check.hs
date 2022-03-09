module Vehicle.Check
  ( CheckOptions(..)
  , check
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import Vehicle.Prelude
import Vehicle.Verify.VerificationStatus
import Vehicle.NeuralNetwork
import Data.Maybe (fromMaybe)
import Control.Exception (catch, IOException)

--------------------------------------------------------------------------------
-- Checking

newtype CheckOptions = CheckOptions
  { databaseFile :: FilePath
  } deriving (Show)

check :: LoggingOptions -> CheckOptions -> IO ()
check loggingOptions checkOptions = do
  -- If the user has specificed no logging target for check mode then
  -- default to command-line.
  let logFile = fromMaybe Nothing (logLocation loggingOptions)
  status <- checkStatus loggingOptions checkOptions
  flushLogger logFile $ logOutput $ pretty status

checkStatus :: LoggingOptions -> CheckOptions -> IO VerificationStatus
checkStatus _loggingOptions CheckOptions{..} = do
  SpecificationStatus{..} <- readSpecificationStatus databaseFile
  case status of
    Verified -> do
      (missingNetworks, alteredNetworks) <- checkIntegrityOfNetworks networkInfo
      return $ case missingNetworks of
        x : xs -> NetworksMissing (x :| xs)
        []     -> case alteredNetworks of
          x : xs -> NetworksAltered (x :| xs)
          []     -> Verified

    _ -> return status

checkIntegrityOfNetworks :: [NetworkVerificationInfo]
                         -> IO ([MissingNetwork], [AlteredNetwork])
checkIntegrityOfNetworks [] = return ([], [])
checkIntegrityOfNetworks (NetworkVerificationInfo{..} : xs) = do
  (missing, altered) <- checkIntegrityOfNetworks xs
  networkStatus <- getNetworkStatus
  return $ case networkStatus of
    Nothing        -> (missing, altered)
    Just (Left m)  -> (m : missing, altered)
    Just (Right a) -> (missing, a : altered)
  where
    getNetworkStatus :: IO (Maybe (Either MissingNetwork AlteredNetwork))
    getNetworkStatus = do
      maybeNewHash <- catch @IOException (Just <$> hashNetwork location) (const $ return Nothing)
      case maybeNewHash of
        Nothing -> return $ Just $ Left $ MissingNetwork name location
        Just newHash ->
          if networkHash == newHash then
            return Nothing
          else
            return $ Just $ Right $ AlteredNetwork name location