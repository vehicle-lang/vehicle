import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.FilePath.Glob (globDir)
import Vehicle.Test.Golden (readTestTree)
import Test.Tasty (defaultMain)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

testDirectory :: FilePath
testDirectory = "tests" </> "golden"

main :: IO ()
main = do
  setLocaleEncoding utf8
  testTree <- readTestTree (testDirectory </> "compile" </> "andGate" </> "andGate.test.json")
  defaultMain testTree