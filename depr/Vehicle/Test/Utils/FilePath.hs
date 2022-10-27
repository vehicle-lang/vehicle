module Vehicle.Test.Utils.FilePath
  ( removeFilePaths
  , filepathTests
  ) where

import Control.Exception (assert)
import Text.Regex.TDFA

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | We often need to remove file paths from golden tests because the file
-- path may be different on different machines. This utility uses a regex
-- to do so.
removeFilePaths :: String -> String
removeFilePaths x =
  let matches = getAllMatches (x =~ filepathRegex) :: [(Int, Int)] in
  foldr sliceOut x matches

filepathRegex :: String
filepathRegex = drive <> folders <> fileName
  where
    drive      = "(([A-Z]:)?[\\//])?"
    name       = "[a-zA-Z\\d$_-]+" -- Note hyphen must be the last character in the class
    extension  = "[a-zA-Z\\d]+"
    folder     = "(" <> name <> "[\\//])"
    folders    = folder <> "+"
    fileName   = name <> "(\\." <> extension <> ")?"

sliceOut :: (Int, Int) -> String -> String
sliceOut (start, size) xs = take start xs <> drop (start + size) xs

filepathTests :: TestTree
filepathTests = testGroup "FilepathRemovalTests"
  [ removeTest "linuxFile"
      "missing file: 'x/y/z.idx'"
      "missing file: ''"

  , removeTest "windowsFile"
      "missing file: 'x\\y\\z.idx'"
      "missing file: ''"

  , removeTest "linuxFolder"
      "missing file: 'x/y/z'"
      "missing file: ''"

  , removeTest "windowsFolder"
      "missing file: 'x\\y\\z'"
      "missing file: ''"

  , removeTest "multiFolder"
      "missing files: 'x/y/z' and 'y/z/d.idx'"
      "missing files: '' and ''"

  , removeTest "drive"
      "missing files: 'D:/x/y/z'"
      "missing files: ''"

  , removeTest "dollarRemoved"
      "missing files: '/x/$y/z'"
      "missing files: ''"

  , removeTest "underscoreRemoved"
      "missing files: '/x/_y/z'"
      "missing files: ''"

  , removeTest "hyphenRemoved"
      "missing files: '/x/s-y/z'"
      "missing files: ''"

  , removeTest "divisionNotRemoved"
      "invalid operator / in"
      "invalid operator / in"

  , removeTest "slashNotRemoved"
      "invalid operator \\ in"
      "invalid operator \\ in"
  ]

removeTest :: String -> String -> String -> TestTree
removeTest name input expected = testCase name $
  assertEqual "" expected (removeFilePaths input)
