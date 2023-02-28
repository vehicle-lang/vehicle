module Vehicle.Compile.ObjectFile
  ( readObjectFile,
    writeObjectFile,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString qualified as BIO
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize, decode, encode)
import GHC.Generics (Generic)
import System.FilePath (dropExtension)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard

data ObjectFileContents = ObjectFileContents
  { _fileHash :: Int,
    _typeResult :: StandardGluedProg
  }
  deriving (Generic)

instance Serialize ObjectFileContents

getObjectFileFromSpecificationFile :: FilePath -> FilePath
getObjectFileFromSpecificationFile specFile =
  dropExtension specFile <> vehicleObjectFileExtension

readObjectFile ::
  (MonadLogger m, MonadIO m) =>
  FilePath ->
  SpecificationText ->
  m (Maybe StandardGluedProg)
readObjectFile specificationFile spec = do
  let interfaceFile = getObjectFileFromSpecificationFile specificationFile
  errorOrContents <- liftIO $ do
    (Right <$> BIO.readFile interfaceFile) `catch` \(e :: IOException) -> return (Left e)

  case errorOrContents of
    Left _error -> do
      logDebug MinDetail $ "No interface file found for" <+> quotePretty specificationFile
      return Nothing
    Right contents -> case decode contents of
      Left _ -> do
        logDebug MinDetail $ "Unable to restore found interface file for" <+> quotePretty specificationFile
        return Nothing
      Right (ObjectFileContents specHash result)
        | specHash /= hash spec -> do
            logDebug MinDetail $ "Outdated interface file found for" <+> quotePretty specificationFile
            return Nothing
        | otherwise -> do
            logDebug MinDetail $ "Loaded interface file for" <+> quotePretty specificationFile
            return $ Just result

writeObjectFile ::
  MonadIO m =>
  FilePath ->
  SpecificationText ->
  StandardGluedProg ->
  m ()
writeObjectFile specificationFile spec result = do
  let interfaceFile = getObjectFileFromSpecificationFile specificationFile
  let specHash = hash spec
  let contents = ObjectFileContents specHash result
  liftIO $ BIO.writeFile interfaceFile (encode contents)
