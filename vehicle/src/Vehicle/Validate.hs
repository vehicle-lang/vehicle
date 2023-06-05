module Vehicle.Validate
  ( ValidateOptions (..),
    validate,
  )
where

import Control.Monad.Trans (MonadIO (liftIO))
import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Prelude
import Vehicle.Resource
import Vehicle.Verify.ProofCache (ProofCache (..), readProofCache)
import Vehicle.Verify.Specification.Status (isVerified)

--------------------------------------------------------------------------------
-- Proof validation

newtype ValidateOptions = ValidateOptions
  { proofCache :: FilePath
  }
  deriving (Eq, Show)

validate :: LoggingSettings -> ValidateOptions -> IO ()
validate loggingSettings checkOptions = runImmediateLogger loggingSettings $ do
  -- If the user has specified no logging target for check mode then
  -- default to command-line.
  status <- checkStatus checkOptions
  programOutput $ pretty status

checkStatus :: ValidateOptions -> ImmediateLoggerT IO ValidateResult
checkStatus ValidateOptions {..} = do
  ProofCache {..} <- liftIO $ readProofCache proofCache
  (missingResources, alteredResources) <- checkIntegrityOfResources resourcesIntegrityInfo
  return $ case (missingResources, alteredResources, isVerified status) of
    (x : xs, _, _) -> MissingResources (x :| xs)
    ([], x : xs, _) -> AlteredResources (x :| xs)
    ([], [], False) -> Unverified
    ([], [], True) -> Verified

data ValidateResult
  = Verified
  | Unverified
  | MissingResources (NonEmpty ResourceIntegrityInfo)
  | AlteredResources (NonEmpty ResourceIntegrityInfo)

instance Pretty ValidateResult where
  pretty Verified = "Status: verified"
  pretty Unverified = "Status: unverified"
  pretty (MissingResources missingResources) =
    "Status: unknown"
      <> line
      <> line
      <> "The following cannot not be found:"
      <> line
      <> line
      <> indent 2 (vsep (fmap prettyResource missingResources))
      <> line
      <> line
      <> "To fix this problem, either move the missing files back to"
        <+> "the"
        <+> locations
        <+> "above or use Vehicle to reverify the"
        <+> "specification with the new"
        <+> locations
      <> "."
    where
      locations = "location" <> if length missingResources == 1 then "" else "s"
  pretty (AlteredResources alteredResources) =
    "Status: unknown"
      <> line
      <> line
      <> "The following have been altered since verification was"
        <+> "last run:"
      <> line
      <> line
      <> indent 2 (vsep (fmap prettyResource alteredResources))
      <> line
      <> line
      <> "To fix this problem, use Vehicle to reverify the specification."

prettyResource :: ResourceIntegrityInfo -> Doc ann
prettyResource ResourceIntegrityInfo {..} =
  pretty name <+> parens (pretty filePath)
