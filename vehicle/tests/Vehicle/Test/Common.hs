module Vehicle.Test.Common where

import Data.Text (Text)
import Paths_vehicle (getDataDir)
import System.FilePath ((</>))
import Vehicle.Backend.Prelude (Backend)

data TestLocation
  = Tests
  | Examples

data TestSpec = TestSpec
    { testName       :: String
    , testLocation   :: TestLocation
    , testTargets    :: [Backend]
    , testNetworks   :: [(Text, FilePath)]
    , testDatasets   :: [(Text, FilePath)]
    , testParameters :: [(Text, String)]
    , testDecls      :: [Text]
    }

getTestLocation :: TestLocation -> IO FilePath
getTestLocation Tests    = fmap (</> "tests") getDataDir
getTestLocation Examples = fmap (</> "examples") getDataDir
