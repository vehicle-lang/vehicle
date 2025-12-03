{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Vehicle.Libraries
  ( LibraryName,
    LibraryInfo (..),
    Library (..),
    findModuleFile,
  )
where

import Control.Exception
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BIO
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath, (<.>), (</>))
import Vehicle.Libraries.Core
import Vehicle.Prelude
import Vehicle.Prelude.Logging

installLibrary :: (MonadIO m, MonadLogger m) => Library -> m ()
installLibrary Library {..} = do
  let name = libraryName libraryInfo
  logCompilerSection2 MidDetail "installing library" $ do
    libraryFolder <- getLibraryPath name
    liftIO $ createDirectoryIfMissing True libraryFolder

    -- Write the library info file out
    let libraryInfoFile = getLibraryInfoFile libraryFolder
    let libraryInfoFileContent = encodePretty libraryInfo
    liftIO $ BIO.writeFile libraryInfoFile libraryInfoFileContent

    -- Write the modules in the library out
    forM_ (Map.toList libraryContent) $
      installModule libraryFolder

installModule :: (MonadIO m, MonadLogger m) => FilePath -> (Module, Text) -> m ()
installModule libraryFolder (modul, content) = do
  let libraryContentFile = libraryFolder </> moduleFilePath modul
  logDebug MidDetail $ "Installing" <+> quotePretty modul <+> "to" <+> quotePretty libraryContentFile
  liftIO $ TIO.writeFile libraryContentFile content

-- | Finds the file path to the requested module.
findModuleFile :: (MonadIO m, MonadLogger m) => Library -> Module -> m FilePath
findModuleFile library modul = do
  libraryFolder <- ensureLatestVersionOfLibraryInstalled library
  ensureLatestVersionOfModuleInstalled library modul
  return $ libraryFolder </> moduleFilePath modul

-- | Checks that the library is up-to-date and if not, installs the latest one.
ensureLatestVersionOfLibraryInstalled :: (MonadIO m, MonadLogger m) => Library -> m FilePath
ensureLatestVersionOfLibraryInstalled library@Library {..} = do
  let name = libraryName libraryInfo
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
      logDebug MidDetail $ "Unable to find or read" <+> quotePretty libraryInfoFile
      return False
    Right contents -> case decode contents of
      Nothing -> do
        logDebug MidDetail $ "Unable to decode contents of" <+> quotePretty libraryInfoFile
        return False
      Just actualInfo -> do
        let actualVersion = libraryVersion actualInfo
        let expectedVersion = libraryVersion libraryInfo
        let versionsMatch = actualVersion == expectedVersion

        logDebug MidDetail $
          if versionsMatch
            then
              "Found up-to-date installed version of"
                <+> quotePretty name
                <+> "at"
                <+> quotePretty libraryFolder
            else
              "Installed version of"
                <+> quotePretty name
                <+> parens (pretty actualVersion)
                <+> "does not match latest version"
                <+> parens (pretty expectedVersion)

        return versionsMatch

  -- If not update to date then reinstall
  unless libraryUpToDate $ do
    installLibrary library

  return libraryFolder

moduleFilePath :: Module -> FilePath
moduleFilePath (Module names) = joinPath names <.> specificationFileExtension

#ifdef releaseBuild
ensureLatestVersionOfModuleInstalled :: (MonadLogger m, MonadIO m) => Library -> Module -> m ()
ensureLatestVersionOfModuleInstalled _library _modul = return ()
#else
-- If non-release build then check that the file actually matches as well
-- in case the developer has changed them.
ensureLatestVersionOfModuleInstalled :: (MonadLogger m, MonadIO m) => Library -> Module -> m ()
ensureLatestVersionOfModuleInstalled Library {..} modul = do
  libraryFolder <- getLibraryPath (libraryName libraryInfo)

  expectedFileContents <- case Map.lookup modul libraryContent of
    Nothing -> developerError ("Missing module" <+> quotePretty modul)
    Just expectedContents -> return expectedContents

  matchesExpectedContents <- liftIO $ handle (\(_e :: IOException) -> return False) $ do
    actualFileContents <- TIO.readFile (libraryFolder </> moduleFilePath modul)
    return $ actualFileContents == expectedFileContents

  unless matchesExpectedContents $ do
    logDebug MidDetail $ "Found dirty copy of" <+> quotePretty (moduleFilePath modul)
    installModule libraryFolder (modul, expectedFileContents)
#endif
