module Vehicle.Libraries.Core
  ( Library (..),
    LibraryName,
    LibraryContent,
    calculateModuleFilePath,
    calculateLibraryFilePath,
    ResolvedLibrary (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.FilePath (joinPath, (<.>), (</>))
import Vehicle.Prelude

type LibraryName = String

--------------------------------------------------------------------------------
-- The file format on disk for library files

data Library = Library
  { libraryName :: LibraryName,
    libraryVersion :: VersionString,
    libraryModules :: [ModulePath]
  }
  deriving (Generic)

instance FromJSON Library

instance ToJSON Library

--------------------------------------------------------------------------------
-- The format used internally in the compiler

-- | Information about a parsed library file
newtype ResolvedLibrary = ResolvedLibrary
  { resolvedModules :: [(ModulePath, FilePath)]
  }
  deriving (Generic)

type LibraryContent = Map ModulePath Text

calculateLibraryFilePath :: FilePath -> FilePath
calculateLibraryFilePath libraryLocation =
  libraryLocation </> vehicleLibraryExtension

calculateModuleFilePath :: FilePath -> ModulePath -> FilePath
calculateModuleFilePath libraryLocation (ModulePath path) =
  libraryLocation </> joinPath path <.> specificationFileExtension
