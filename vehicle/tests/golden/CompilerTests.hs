module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>))
import Test.Tasty (defaultMain)
import Vehicle.Test.Golden (makeTestTreeFromDirectoryRecursive)

testDirectory :: FilePath
testDirectory = "tests" </> "golden"

main :: IO ()
main = do
  setLocaleEncoding utf8
  testTree <- makeTestTreeFromDirectoryRecursive "Compiler" testDirectory
  defaultMain testTree
