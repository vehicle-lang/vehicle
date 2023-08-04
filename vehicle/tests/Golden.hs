import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>))
import Test.Tasty (defaultMain)
import Test.Tasty.Golden.Executable (IgnoreFiles (..), SomeOption (..), makeTestTreeFromDirectoryRecursive)

testDirectory :: FilePath
testDirectory = "tests" </> "golden"

options :: [SomeOption]
options =
  [ AppendOption (IgnoreFiles ["*.vclo", "**/*.vclo", ".vcl-plan", "**/.vcl-plan"])
  ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  testTree <- makeTestTreeFromDirectoryRecursive options "Compiler" testDirectory
  defaultMain testTree
