module Vehicle.Backend.VNNLib.Interact
  ( writeOutProperty
  ) where

import Vehicle.Backend.Prelude
import Vehicle.Backend.VNNLib.Core

writeOutProperty :: Maybe FilePath -> VNNLibProperty -> IO ()
writeOutProperty filepath property =
  writeResultToFile VNNLibBackend filepath (doc property)