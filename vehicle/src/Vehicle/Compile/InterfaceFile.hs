
module Vehicle.Compile.InterfaceFile
  ( readInterfaceFile
  , writeInterfaceFile
  ) where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON (..), FromJSON, encode, decode)
import Data.ByteString.Lazy qualified as BIO
import Control.Exception
import System.FilePath (dropExtension)
import GHC.Generics (Generic)

import Vehicle.Compile.Prelude
import Vehicle.Compile.Type (TypeCheckingResult(..))
import Data.Hashable (Hashable(..))

data InterfaceContents = InterfaceContents
  { _fileHash   :: Int
  , _typeResult :: TypeCheckingResult
  } deriving (Generic)

instance ToJSON   InterfaceContents
instance FromJSON InterfaceContents

getInterfaceFileFromSpecificationFile :: FilePath -> FilePath
getInterfaceFileFromSpecificationFile specFile =
  dropExtension specFile <> vehicleInterfaceFileExtension

readInterfaceFile :: (MonadLogger m, MonadIO m)
                  => FilePath
                  -> SpecificationText
                  -> m (Maybe TypeCheckingResult)
readInterfaceFile specificationFile spec = do
  let interfaceFile = getInterfaceFileFromSpecificationFile specificationFile
  errorOrContents <- liftIO $ do
    (Right <$> BIO.readFile interfaceFile) `catch` \ (e :: IOException) -> return (Left e)

  case errorOrContents of
    Left _error -> do
      logDebug MinDetail $ "No interface file found for" <+> quotePretty specificationFile
      return Nothing

    Right contents -> case decode contents of
      Nothing -> do
        logDebug MinDetail $ "Unable to restore found interface file for" <+> quotePretty specificationFile
        return Nothing

      Just (InterfaceContents specHash result)
        | specHash /= hash spec -> do
          logDebug MinDetail $ "Outdated interface file found for" <+> quotePretty specificationFile
          return Nothing

        | otherwise -> do
          logDebug MinDetail $ "Loaded interface file for" <+> quotePretty specificationFile
          return $ Just result

writeInterfaceFile :: MonadIO m
                   => FilePath
                   -> SpecificationText
                   -> TypeCheckingResult
                   -> m ()
writeInterfaceFile specificationFile spec result = do
  let interfaceFile = getInterfaceFileFromSpecificationFile specificationFile
  let specHash = hash spec
  let contents = InterfaceContents specHash result
  liftIO $ BIO.writeFile interfaceFile (encode contents)
