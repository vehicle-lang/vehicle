module Vehicle.Libraries.Core where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
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
