module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.FilePath ((</>))
import Test.Tasty (defaultMain)
import Vehicle.Test.Golden (TestFilter, makeTestTreeFromDirectoryRecursive)

testDirectory :: FilePath
testDirectory = "tests" </> "golden"

main :: IO ()
main = do
  setLocaleEncoding utf8
  testTree <- makeTestTreeFromDirectoryRecursive "Integration" isIntegrationTest testDirectory
  defaultMain testTree

isIntegrationTest :: TestFilter
isIntegrationTest s = s == "MarabouVerify"
