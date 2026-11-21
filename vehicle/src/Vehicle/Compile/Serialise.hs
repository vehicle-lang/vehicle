module Vehicle.Compile.Serialise
  ( CachedModule (..),
    readObjectFile,
    decodeCachedModuleInterface,
    writeObjectFile,
  )
where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Hashable (Hashable (..))
import Data.Serialize (Serialize, decode, encode)
import GHC.Generics (Generic)
import System.FilePath (dropExtension)
import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Standard
import Vehicle.Data.Code.ModuleInterface (ModuleInterface (..))

--------------------------------------------------------------------------------
-- Object file format

-- | The contents of an object file. The interface itself is stored as a
-- separately encoded `ByteString` so that the cheap header can be decoded and
-- checked (source hash, imports) without paying the cost of decoding
-- the (potentially very large) interface. These need to be checked to see if
-- the file or any of its imports have changed. The interface is only decoded
-- once we know it is going to be used.
data ObjectFileContents = ObjectFileContents
  { fileHash :: Int,
    fileImports :: [ImportStatement],
    encodedInterface :: ByteString
  }
  deriving stock (Generic)
  deriving anyclass (Serialize)

-- | A module whose object file is present and up-to-date with respect to its
-- source, but whose interface has not yet been decoded.
data CachedModule = CachedModule
  { cachedImports :: [ImportStatement],
    cachedEncodedInterface :: ByteString
  }

getObjectFileFromSpecificationFile :: FilePath -> FilePath
getObjectFileFromSpecificationFile specFile =
  dropExtension specFile <> vehicleObjectFileExtension

--------------------------------------------------------------------------------
-- Reading

-- | Reads the header of the object file for the specification, returning
-- `Nothing` if it is missing or out of date. Does not decode the interface:
-- use `decodeCachedModuleInterface` for that.
readObjectFile ::
  (MonadLogger m, MonadIO m) =>
  FilePath ->
  ModuleText ->
  m (Maybe CachedModule)
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
          <> line
      return Nothing
    InexplicableDecodingError err -> do
      logDebug MinDetail $
        errString
          <+> "as it was unreadable for the unexpected reason:"
          <> line
          <> indent 2 (pretty err)
          <> line
      return Nothing
    VersionMismatchError currentVersion writtenVersion -> do
      logDebug MinDetail $
        errString
          <+> "as it was written by an incompatible build of Vehicle"
          <+> parens ("cache key" <+> pretty writtenVersion)
          <+> "and the current build has cache key"
          <+> pretty currentVersion
          <> "."
      return Nothing
    SuccessfulDecoding ObjectFileContents {..}
      | fileHash /= hash spec -> do
          logDebug MinDetail $ "Outdated interface file found for" <+> quotePretty specificationFile <> line
          return Nothing
      | otherwise -> do
          logDebug MinDetail $ "Found up-to-date interface file for" <+> quotePretty specificationFile
          return $
            Just
              CachedModule
                { cachedImports = fileImports,
                  cachedEncodedInterface = encodedInterface
                }

-- | Decodes the interface of a cached module, returning `Nothing` if it
-- could not be decoded.
decodeCachedModuleInterface ::
  (MonadLogger m) =>
  FilePath ->
  CachedModule ->
  m (Maybe (ModuleInterface Builtin))
decodeCachedModuleInterface specificationFile CachedModule {..} =
  case decode cachedEncodedInterface of
    Left err -> do
      logDebug MinDetail $
        "Unable to restore interface file"
          <+> quotePretty specificationFile
          <+> "as the interface was unreadable for the unexpected reason:"
          <> line
          <> indent 2 (pretty err)
          <> line
      return Nothing
    Right moduleInterface -> do
      logDebug MinDetail $ "Loaded interface file for" <+> quotePretty specificationFile
      return $ Just moduleInterface

--------------------------------------------------------------------------------
-- Writing

writeObjectFile ::
  (MonadIO m) =>
  FilePath ->
  ModuleText ->
  ModuleInterface Builtin ->
  m ()
writeObjectFile specificationFile spec result = do
  let interfaceFile = getObjectFileFromSpecificationFile specificationFile
  let Module imports _ = typedModule result
  let contents =
        ObjectFileContents
          { fileHash = hash spec,
            fileImports = imports,
            encodedInterface = encode result
          }
  encodeAndWriteVersioned interfaceFile contents
