import System.FilePath ((</>))
import Vehicle.Test.Golden (makeTestTreeFromDirectoryRecursive)
import Test.Tasty (defaultMain)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

testDirectory :: FilePath
testDirectory = "tests" </> "golden"

main :: IO ()
main = do
  setLocaleEncoding utf8
  testTree <- makeTestTreeFromDirectoryRecursive "Tests" testDirectory
  defaultMain testTree
