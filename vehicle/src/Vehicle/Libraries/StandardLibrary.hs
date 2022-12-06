
module Vehicle.Libraries.StandardLibrary
  ( standardLibrary
  ) where

import Data.Text (Text)
import Data.Version (Version)

import Vehicle.Libraries

stdlibName :: LibraryName
stdlibName = "stdlib"

stdlibVersion :: Version
stdlibVersion = [0,3]

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
{-
\n\
  \bigAnd : Vector Bool n -> Bool\n\
  \bigAnd = fold (\\x y -> x and y) True\n\
  \n\
  \bigOr : Vector Bool n -> Bool\n\
  \bigOr = fold (\\x y -> x or y) False\n\
-}
