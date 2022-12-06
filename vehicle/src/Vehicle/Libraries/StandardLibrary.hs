
module Vehicle.Libraries.StandardLibrary
  ( standardLibrary
  ) where

import Data.Text (Text)
import Data.Version (Version)

import Vehicle.Libraries

stdlibName :: LibraryName
stdlibName = "stdlib"

stdlibVersion :: Version
stdlibVersion = [0,2]

standardLibrary :: Library
standardLibrary = Library
  { libraryInfo    = LibraryInfo
    { libraryName    = stdlibName
    , libraryVersion = stdlibVersion
    }
  , libraryContent = content
  }

content :: Text
content = "\
  \zipWith : (A -> B -> C) -> Vector A n -> Vector B n -> Vector C n\n\
  \zipWith f xs ys = foreach i . f (xs ! i) (ys ! i)\n\
  \"
