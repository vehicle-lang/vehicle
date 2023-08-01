module Vehicle.Validate
  ( ValidateOptions (..),
    validate,
  )
where

import Control.Monad (forM)
import Control.Monad.Trans (MonadIO (liftIO))
import Vehicle.Prelude
import Vehicle.Resource
import Vehicle.Verify.Specification (SpecificationCacheIndex (..), multiPropertyAddresses, properties)
import Vehicle.Verify.Specification.IO (readPropertyResult, readSpecificationCacheIndex)

--------------------------------------------------------------------------------
-- Proof validation

newtype ValidateOptions = ValidateOptions
  { verificationCache :: FilePath
  }
  deriving (Eq, Show)

validate :: LoggingSettings -> ValidateOptions -> IO ()
validate loggingSettings checkOptions = runImmediateLogger loggingSettings $ do
  -- If the user has specified no logging target for check mode then
  -- default to command-line.
  status <- checkSpecificationStatus checkOptions
  programOutput $ pretty status

checkSpecificationStatus ::
  (MonadIO m, MonadLogger m) =>
  ValidateOptions ->
  m ValidateResult
checkSpecificationStatus ValidateOptions {..} = do
  SpecificationCacheIndex {..} <- liftIO $ readSpecificationCacheIndex verificationCache
  maybeIntegrityError <- checkIntegrityOfResources resourcesIntegrityInfo
  case maybeIntegrityError of
    Just err -> return $ IntegrityError err
    Nothing -> do
      let propertyAddresses = concatMap (multiPropertyAddresses . snd) properties
      statuses <- forM propertyAddresses $ readPropertyResult verificationCache
      if and statuses
        then return Verified
        else return Unverified

data ValidateResult
  = Verified
  | Unverified
  | IntegrityError ResourceIntegrityError

instance Pretty ValidateResult where
  pretty Verified = "Status: verified"
  pretty Unverified = "Status: unverified"
  pretty (IntegrityError err) = "Status: unknown" <> line <> line <> pretty err
