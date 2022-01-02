module Vehicle.Check 
  ( CheckOptions(..)
  , check
  ) where

import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Checking

data CheckOptions = CheckOptions
  { databaseFile :: FilePath
  , propertyUUID :: String
  } deriving (Show)

check :: OutputFilePaths -> CheckOptions -> IO ()
check _logFile _checkOptions = do
  print ("Valid" :: String)
  return ()