module Vehicle.Backend.Agda.Interact
  ( writeAgdaFile
  ) where

import Vehicle.Prelude
import Vehicle.Backend.Prelude

writeAgdaFile :: Maybe FilePath -> Doc a -> IO ()
writeAgdaFile = writeResultToFile AgdaBackend