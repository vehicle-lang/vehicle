module Vehicle.Backend.Agda.Interact
  ( writeOutProperty
  ) where

import Vehicle.Prelude
import Vehicle.Backend.Prelude

writeOutProperty :: Maybe FilePath -> Doc a -> IO ()
writeOutProperty = writeResultToFile AgdaBackend