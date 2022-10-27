module Vehicle.Backend.Agda.Interact
  ( writeAgdaFile
  ) where

import Vehicle.Backend.Prelude
import Vehicle.Prelude

writeAgdaFile :: Maybe FilePath -> Doc a -> IO ()
writeAgdaFile = writeResultToFile AgdaBackend
