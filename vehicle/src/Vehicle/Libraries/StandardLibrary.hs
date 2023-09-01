{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Libraries.StandardLibrary
  ( standardLibrary,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text.Encoding (decodeUtf8)
import Vehicle.Libraries
import Vehicle.Prelude

standardLibraryContentBS :: ByteString
standardLibraryContentBS = $(makeRelativeToProject "lib/std.vcl" >>= embedFile)

standardLibrary :: Library
standardLibrary =
  Library
    { libraryInfo =
        LibraryInfo
          { libraryName = "std",
            libraryVersion = preciseVehicleVersion
          },
      libraryContent = decodeUtf8 standardLibraryContentBS
    }
