module Vehicle.Check
  ( CheckOptions(..)
  , check
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Control.Exception (catch, IOException)
import Control.Monad.Trans (MonadIO(liftIO))

import Vehicle.Prelude
import Vehicle.Verify.VerificationStatus (readProofCache, isSpecVerified, ProofCache(..), NetworkVerificationInfo(..))
import Vehicle.NeuralNetwork

--------------------------------------------------------------------------------
-- Checking

newtype CheckOptions = CheckOptions
  { proofCache :: FilePath
  } deriving (Show)

check :: LoggingOptions -> CheckOptions -> IO ()
check loggingOptions checkOptions = fromLoggerTIO loggingOptions $ do
  -- If the user has specificed no logging target for check mode then
  -- default to command-line.
  status <- checkStatus loggingOptions checkOptions
  programOutput loggingOptions $ pretty status

checkStatus :: LoggingOptions -> CheckOptions -> LoggerT IO CheckResult
checkStatus _loggingOptions CheckOptions{..} = do
  ProofCache{..} <- liftIO $ readProofCache proofCache
  (missingNetworks, alteredNetworks) <- checkIntegrityOfNetworks networkInfo
  return $ case (missingNetworks, alteredNetworks, isSpecVerified status) of
    (x : xs, _, _)  -> NetworksMissing (x :| xs)
    ([], x : xs, _) -> NetworksAltered (x :| xs)
    ([], [], False) -> Unverified
    ([], [], True)  -> Verified

checkIntegrityOfNetworks :: [NetworkVerificationInfo]
                         -> LoggerT IO ([MissingNetwork], [AlteredNetwork])
checkIntegrityOfNetworks [] = return ([], [])
checkIntegrityOfNetworks (NetworkVerificationInfo{..} : xs) = do
  (missing, altered) <- checkIntegrityOfNetworks xs
  networkStatus <- liftIO getNetworkStatus
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

data CheckResult
  = Verified
  | Unverified
  | NetworksMissing (NonEmpty MissingNetwork)
  | NetworksAltered (NonEmpty AlteredNetwork)

instance Pretty CheckResult where
  pretty Verified = "Status: verified"

  pretty Unverified = "Status: unverified"

  pretty (NetworksMissing missingNetworks) =
    "Status: unknown" <> line <> line <>
    "The following networks cannot not be found:" <> line <> line <>
      indent 2 (vsep (fmap pretty missingNetworks)) <> line <> line <>
    "To fix this problem, either move the missing files back to" <+>
    "the" <+> locations <+> "above or use Vehicle to reverify the" <+>
    "specification with the new" <+> locations <> "."
    where
      locations = "location" <> if length missingNetworks == 1 then "" else "s"

  pretty (NetworksAltered alteredNetworks) =
    "Status: unknown" <> line <> line <>
    "The following networks have been altered since verification was" <+>
    "last run:" <> line <> line <>
    indent 2 (vsep (fmap pretty alteredNetworks)) <> line <> line <>
    "To fix this problem, use Vehicle to reverify the specification."


--------------------------------------------------------------------------------
-- Missing networks

data MissingNetwork = MissingNetwork Text FilePath

instance Pretty MissingNetwork where
  pretty (MissingNetwork name filepath) =
    pretty name <+> parens (pretty filepath)

--------------------------------------------------------------------------------
-- Altered networks

data AlteredNetwork = AlteredNetwork Text FilePath

instance Pretty AlteredNetwork where
  pretty (AlteredNetwork name filepath) =
    pretty name <+> parens (pretty filepath)