module Vehicle.Compile.ObjectFile
  ( readObjectFile,
    writeObjectFile,
  )
where

import Control.Monad.IO.Class
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.FilePath (dropExtension)
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Subsystem.Standard

data ObjectFileContents = ObjectFileContents
  { fileHash :: Int,
    typeResult :: StandardGluedProg
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
  errorOrContents <- readAndDecodeVersioned interfaceFile

  let errString = "Unable to restore interface file" <+> quotePretty specificationFile
  case errorOrContents of
    IOError err -> do
      logDebug MinDetail $
        errString
          <+> "as the following IO error was thrown:"
          <> line
          <> indent 2 (pretty $ show err)
      return Nothing
    InexplicableDecodingError err -> do
      logDebug MinDetail $
        errString
          <+> "as it was unreadable for the unexpected reason:"
          <> line
          <> indent 2 (pretty err)
      return Nothing
    VersionMismatchError currentVersion writtenVersion err -> do
      logDebug MinDetail $
        errString
          <+> "as it was written with Vehicle version"
          <+> pretty writtenVersion
          <+> "which is apparently unreadable with the current Vehicle version"
          <+> pretty currentVersion
          <> "."
            <+> "In particular decoding threw the error:"
          <> line
          <> indent 2 (pretty err)
      return Nothing
    SuccessfulDecoding ObjectFileContents {..}
      | fileHash /= hash spec -> do
          logDebug MinDetail $ "Outdated interface file found for" <+> quotePretty specificationFile
          return Nothing
      | otherwise -> do
          logDebug MinDetail $ "Loaded interface file for" <+> quotePretty specificationFile
          return $ Just typeResult

writeObjectFile ::
  (MonadIO m) =>
  FilePath ->
  SpecificationText ->
  StandardGluedProg ->
  m ()
writeObjectFile specificationFile spec result = do
  let interfaceFile = getObjectFileFromSpecificationFile specificationFile
  let specHash = hash spec
  let contents = ObjectFileContents specHash result
  encodeAndWriteVersioned interfaceFile contents
