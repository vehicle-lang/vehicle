module Vehicle.Compile.ObjectFile
  ( readObjectFile
  , writeObjectFile
  ) where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON (..), FromJSON, encode, decode)
import Data.ByteString.Lazy qualified as BIO
import Control.Exception
import System.FilePath (dropExtension)
import GHC.Generics (Generic)

import Vehicle.Compile.Prelude
import Data.Hashable (Hashable(..))

data ObjectFileContents = ObjectFileContents
  { _fileHash   :: Int
  , _typeResult :: TypedProg
  } deriving (Generic)

instance ToJSON   ObjectFileContents
instance FromJSON ObjectFileContents

getObjectFileFromSpecificationFile :: FilePath -> FilePath
getObjectFileFromSpecificationFile specFile =
  dropExtension specFile <> vehicleObjectFileExtension

readObjectFile :: (MonadLogger m, MonadIO m)
                  => FilePath
                  -> SpecificationText
                  -> m (Maybe TypedProg)
readObjectFile specificationFile spec = do
  let interfaceFile = getObjectFileFromSpecificationFile specificationFile
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

      Just (ObjectFileContents specHash result)
        | specHash /= hash spec -> do
          logDebug MinDetail $ "Outdated interface file found for" <+> quotePretty specificationFile
          return Nothing

        | otherwise -> do
          logDebug MinDetail $ "Loaded interface file for" <+> quotePretty specificationFile
          return $ Just result

writeObjectFile :: MonadIO m
                => FilePath
                -> SpecificationText
                -> TypedProg
                -> m ()
writeObjectFile specificationFile spec result = do
  let interfaceFile = getObjectFileFromSpecificationFile specificationFile
  let specHash = hash spec
  let contents = ObjectFileContents specHash result
  liftIO $ BIO.writeFile interfaceFile (encode contents)
