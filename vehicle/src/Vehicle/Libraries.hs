module Vehicle.Libraries
  ( LibraryName,
    Library (..),
    resolveLibrary,
    getLibraryPath,
    ensureLatestVersionOfLibraryInstalled,
  )
where

import Control.Exception
import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Lazy qualified as BIO
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Vehicle.Libraries.Core
import Vehicle.Prelude
import Vehicle.Prelude.Logging

resolveLibrary :: (MonadIO m, MonadLogger m) => LibraryName -> m ResolvedLibrary
resolveLibrary libraryName = do
  logCompilerSection2 MinDetail ("resolving library" <+> quotePretty libraryName) $ do
    libraryFolder <- getLibraryPath libraryName

    -- Read the library file
    errorOrLibrary <- readLibraryFile libraryFolder
    library <- case errorOrLibrary of
      Left err -> developerError err
      Right library -> return library

    -- Enumerate the modules within it
    let moduleMapping = fmap (\m -> (m, calculateModuleFilePath libraryFolder m)) (libraryModules library)

    logDebug MaxDetail $
      "Found modules:"
        <> lineIndent (prettyMapEntries $ fmap (bimap pretty pretty) moduleMapping)

    return $ ResolvedLibrary moduleMapping

getLibraryPath :: (MonadIO m) => LibraryName -> m FilePath
getLibraryPath name = do
  vehiclePath <- getVehiclePath
  return $ vehiclePath </> "libraries" </> name

readLibraryFile :: (MonadIO m) => FilePath -> m (Either UnAnnDoc Library)
readLibraryFile libraryFolder = runExceptT $ do
  let libraryFile = calculateLibraryFilePath libraryFolder
  errorOrByteString <- liftIO $ catch (Right <$> BIO.readFile libraryFile) (\(e :: IOException) -> return $ Left e)
  case errorOrByteString of
    Left err ->
      throwError $
        "Unable to find or read library file"
          <+> quotePretty libraryFile
          <+> ":"
          <> lineIndent (pretty $ show err)
    Right byteString -> case decode byteString of
      Nothing ->
        throwError $
          "Unabled to decode library file"
            <+> quotePretty libraryFile
            <> "."
      Just plan -> return plan

installLibrary :: (MonadIO m, MonadLogger m) => Library -> LibraryContent -> m ()
installLibrary library@Library {..} libraryContent = do
  let name = libraryName
  logCompilerSection2 MinDetail ("installing library" <+> quotePretty libraryName) $ do
    libraryFolder <- getLibraryPath name
    liftIO $ createDirectoryIfMissing True libraryFolder

    -- Write the library info file out
    let libraryInfoFile = calculateLibraryFilePath libraryFolder
    let libraryInfoFileContent = encodePretty library
    liftIO $ BIO.writeFile libraryInfoFile libraryInfoFileContent

    -- Write the modules in the library out
    forM_ (Map.toList libraryContent) $
      installModule libraryFolder

installModule :: (MonadIO m, MonadLogger m) => FilePath -> (ModulePath, Text) -> m ()
installModule libraryFolder (modul, content) = do
  let libraryContentFile = calculateModuleFilePath libraryFolder modul
  logDebug MidDetail $ "Installing" <+> quotePretty modul <+> "to" <+> quotePretty libraryContentFile
  liftIO $ createDirectoryIfMissing True (takeDirectory libraryContentFile)
  liftIO $ TIO.writeFile libraryContentFile content

-- | Checks that the library is up-to-date and if not, installs the latest one.
ensureLatestVersionOfLibraryInstalled ::
  (MonadIO m, MonadLogger m) =>
  Library ->
  LibraryContent ->
  m ()
ensureLatestVersionOfLibraryInstalled library libraryContent = do
  -- Check the library info file and see if it's up to date
  libraryUpToDate <- do
    libraryFolder <- getLibraryPath (libraryName library)

    errorOrContents <- readLibraryFile libraryFolder
    case errorOrContents of
      Left err -> do
        logDebug MidDetail err
        return False
      Right actualLibrary -> do
        let actualVersion = libraryVersion actualLibrary
        let expectedVersion = libraryVersion library
        let versionsMatch = actualVersion == expectedVersion

        logDebug MidDetail $
          if versionsMatch
            then
              "Found up-to-date installed version of"
                <+> quotePretty (libraryName library)
                <+> "at"
                <+> quotePretty libraryFolder
            else
              "Installed version of"
                <+> quotePretty (libraryName library)
                <+> parens (pretty actualVersion)
                <+> "does not match latest version"
                <+> parens (pretty expectedVersion)

        return versionsMatch

  -- If not update to date then reinstall
  unless libraryUpToDate $ do
    installLibrary library libraryContent
