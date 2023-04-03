{-# LANGUAGE NamedFieldPuns #-}

import Control.Exception (catch, handle, throwIO)
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Traversable (for)
import Distribution.PackageDescription (ComponentName (..), ForeignLib (..), PackageDescription (..))
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (componentBuildDir)
import Distribution.Simple.Setup (BuildFlags (..), fromFlagOrDefault)
import Distribution.Simple.Utils (die', info)
import Distribution.Types.LocalBuildInfo (componentNameCLBIs)
import Distribution.Types.UnqualComponentName (UnqualComponentName, unUnqualComponentName)
import Distribution.Verbosity (Verbosity, normal)
import System.Directory (doesDirectoryExist, copyFile)
import System.Environment (getEnv)
import System.FilePath ((<.>))
import System.FilePath.Posix ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Info (os)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { postBuild = \args buildFlags packageDescription localBuildInfo -> do
          let verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)
          maybeInstallDir <- getInstallDir verbosity
          for_ maybeInstallDir $ \installDir -> do
            let PackageDescription {foreignLibs} = packageDescription
            for_ foreignLibs $ \foreignLib -> do
              let ForeignLib {foreignLibName} = foreignLib
              let foreignLibCLBIs = componentNameCLBIs localBuildInfo (CFLibName foreignLibName)
              for_ foreignLibCLBIs $ \foreignLibCLBI -> do
                let foreignLibBuildDir = componentBuildDir localBuildInfo foreignLibCLBI
                let foreignLibFileName = getForeignLibFileName foreignLibName
                let foreignLibBuildPath = foreignLibBuildDir </> foreignLibFileName
                let foreignLibInstallPath = installDir </> foreignLibFileName
                info verbosity $ "Installing " <> foreignLibFileName <> " to " <> installDir
                copyFile foreignLibBuildPath foreignLibInstallPath
      }

getForeignLibFileName :: UnqualComponentName -> FilePath
getForeignLibFileName foreignLibName
  | System.Info.os == "mingw32" = libName <.> "dll"
  | System.Info.os == "darwin" = "lib" <> libName <.> "dylib"
  | otherwise = "lib" <> libName <.> "so"
  where
    libName = unUnqualComponentName foreignLibName

getInstallDir :: Verbosity -> IO (Maybe FilePath)
getInstallDir verbosity =
  handle doesNotExistErrorHandler unsafeGetInstallDir
  where
    unsafeGetInstallDir = do
      installDir <- getEnv "INSTALLDIR"
      installDirExists <- doesDirectoryExist installDir
      unless installDirExists $
        die' verbosity $
          "INSTALLDIR '" <> installDir <> "' does not exist"
      return $ Just installDir

    doesNotExistErrorHandler e =
      if isDoesNotExistError e then return Nothing else throwIO e
