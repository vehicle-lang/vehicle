module Vehicle.Check
  ( CheckOptions(..)
  , check
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Control.Exception (catch, IOException)
import Control.Monad.Trans (MonadIO(liftIO))

import Vehicle.Prelude
import Vehicle.Verify.VerificationStatus (readProofCache, isSpecVerified, ProofCache(..))
import Vehicle.Resource

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
  (missingNetworks, alteredNetworks) <- checkResourceIntegrity resources
  return $ case (missingNetworks, alteredNetworks, isSpecVerified status) of
    (x : xs, _, _)  -> MissingResources (x :| xs)
    ([], x : xs, _) -> AlteredResources (x :| xs)
    ([], [], False) -> Unverified
    ([], [], True)  -> Verified

getResourceStatus :: ResourceSummary -> IO ResourceStatus
getResourceStatus ResourceSummary{..}= do
  maybeNewHash <- catch @IOException (Just <$> hashResource location) (const $ return Nothing)
  return $ case maybeNewHash of
    Nothing -> Missing
    Just newHash
      | fileHash /= newHash -> Altered
      | otherwise           -> Unchanged

checkResourceIntegrity :: [ResourceSummary] -> LoggerT IO ([ResourceSummary], [ResourceSummary])
checkResourceIntegrity []       = return ([], [])
checkResourceIntegrity (r : rs) = do
  (missing, altered) <- checkResourceIntegrity rs
  resourceStatus <- liftIO (getResourceStatus r)
  return $ case resourceStatus of
    Unchanged -> (missing, altered)
    Altered   -> (missing, r : altered)
    Missing   -> (r : missing, altered)


data ResourceStatus
  = Unchanged
  | Altered
  | Missing

data CheckResult
  = Verified
  | Unverified
  | MissingResources (NonEmpty ResourceSummary)
  | AlteredResources (NonEmpty ResourceSummary)

instance Pretty CheckResult where
  pretty Verified = "Status: verified"

  pretty Unverified = "Status: unverified"

  pretty (MissingResources missingNetworks) =
    "Status: unknown" <> line <> line <>
    "The following cannot not be found:" <> line <> line <>
      indent 2 (vsep (fmap prettyResource missingNetworks)) <> line <> line <>
    "To fix this problem, either move the missing files back to" <+>
    "the" <+> locations <+> "above or use Vehicle to reverify the" <+>
    "specification with the new" <+> locations <> "."
    where
      locations = "location" <> if length missingNetworks == 1 then "" else "s"

  pretty (AlteredResources alteredNetworks) =
    "Status: unknown" <> line <> line <>
    "The following have been altered since verification was" <+>
    "last run:" <> line <> line <>
    indent 2 (vsep (fmap prettyResource alteredNetworks)) <> line <> line <>
    "To fix this problem, use Vehicle to reverify the specification."

prettyResource :: ResourceSummary -> Doc ann
prettyResource ResourceSummary{..} =
    pretty resType <+> pretty name <+> parens (pretty location)
