{-# LANGUAGE TemplateHaskell #-}

module Vehicle.Libraries.StandardLibrary
  ( standardLibrary,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map (Map, fromList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Vehicle.Libraries
import Vehicle.Prelude

standardLibraryContentBS :: Map Module Text
standardLibraryContentBS =
  fromList $
    fmap
      (bimap Module decodeUtf8)
      [ (["Definitions"], $(makeRelativeToProject "lib/Definitions.vcl" >>= embedFile))
      ]

standardLibrary :: Library
standardLibrary =
  Library
    { libraryInfo =
        LibraryInfo
          { libraryName = "std",
            libraryVersion = preciseVehicleVersion
          },
      libraryContent = standardLibraryContentBS
    }
