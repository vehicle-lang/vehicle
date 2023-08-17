module Vehicle.Libraries
  ( LibraryName,
    LibraryInfo (..),
    Library (..),
    findLibraryContentFile,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BIO
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import Vehicle.Prelude

type LibraryName = String

data LibraryInfo = LibraryInfo
  { libraryName :: LibraryName,
    libraryVersion :: VersionString
  }
  deriving (Generic)

instance FromJSON LibraryInfo

instance ToJSON LibraryInfo

data Library = Library
  { libraryInfo :: LibraryInfo,
    libraryContent :: Text
  }

getLibraryPath :: (MonadIO m) => LibraryName -> m FilePath
getLibraryPath name = do
  vehiclePath <- getVehiclePath
  return $ vehiclePath </> "libraries" </> name

getLibraryInfoFile :: FilePath -> FilePath
getLibraryInfoFile libraryFolder = libraryFolder </> vehicleLibraryExtension

getLibraryContentFile :: FilePath -> LibraryName -> FilePath
getLibraryContentFile libraryFolder fileName =
  libraryFolder </> fileName <.> specificationFileExtension

installLibrary :: (MonadIO m, MonadLogger m) => Library -> m ()
installLibrary Library {..} = do
  let name = libraryName libraryInfo
  logDebug MaxDetail $ "Installing library" <+> quotePretty name

  libraryFolder <- getLibraryPath name
  liftIO $ createDirectoryIfMissing True libraryFolder

  -- Write the library info file out
  let libraryInfoFile = getLibraryInfoFile libraryFolder
  let libraryInfoFileContent = encodePretty libraryInfo
  liftIO $ BIO.writeFile libraryInfoFile libraryInfoFileContent

  -- Write the contents of the library out
  let libraryContentFile = libraryFolder </> name <.> specificationFileExtension
  liftIO $ TIO.writeFile libraryContentFile libraryContent

-- | Finds the file path to the library content. At the moment
-- this is very hacky, as it assumes there's a single file per library
-- and that it should install a newer version if out of date.
findLibraryContentFile :: (MonadIO m, MonadLogger m) => Library -> m FilePath
findLibraryContentFile library = do
  let name = libraryName $ libraryInfo library
  libraryFolder <- getLibraryPath name

  -- Check the library info file and see if it's up to date
  let libraryInfoFile = getLibraryInfoFile libraryFolder
  errorOrContents <-
    liftIO $
      catch
        (Right <$> BIO.readFile libraryInfoFile)
        (\(e :: IOException) -> return (Left e))

  libraryUpToDate <- case errorOrContents of
    Left _err -> do
      logDebug MaxDetail $ "Unable to find or read" <+> quotePretty libraryInfoFile
      return False
    Right contents -> case decode contents of
      Nothing -> do
        logDebug MaxDetail $ "Unable to decode contents of" <+> quotePretty libraryInfoFile
        return False
      Just actualInfo -> do
        let actualVersion = libraryVersion actualInfo
        let expectedVersion = libraryVersion (libraryInfo library)
        if actualVersion == expectedVersion
          then do
            logDebug MaxDetail $
              "Found up-to-date installed version of"
                <+> quotePretty name
                <+> "at"
                <+> quotePretty libraryFolder
            return True
          else do
            logDebug MaxDetail $
              "Installed version of"
                <+> quotePretty name
                <+> parens (pretty actualVersion)
                <+> "does not match latest version"
                <+> parens (pretty expectedVersion)
            return False

  -- If not update to date then reinstall
  unless libraryUpToDate $ do
    installLibrary library

  return (getLibraryContentFile libraryFolder name)
