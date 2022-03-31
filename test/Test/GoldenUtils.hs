module Test.GoldenUtils
  ( goldenDirectoryTest
  , goldenFileTest
  ) where

import Test.Tasty
import Test.Tasty.Golden.Advanced
import Data.Text ( Text )
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Map (Map, (\\))
import Data.Map qualified as Map (toAscList, fromList, keysSet, size, lookup, intersectionWith, keys)
import Data.Set qualified as Set

import Data.Algorithm.Diff (Diff, PolyDiff(..), getGroupedDiffBy)
import Data.Algorithm.DiffOutput (ppDiff)

import System.Directory (listDirectory, removeFile, createDirectory, removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath (takeFileName, (</>), takeDirectory)
import Data.List (intercalate, isInfixOf)
import System.IO.Error
import Control.Exception (catch, throwIO)
import Data.Bifunctor (Bifunctor(second, first))

--------------------------------------------------------------------------------
-- General utilities

diffText :: [Text] -> Text -> Text -> Maybe String
diffText ignoreList golden output = do
  let goldenLines  = map T.unpack $ T.lines golden
  let outputLines  = map T.unpack $ T.lines output
  let equalityTest = diffEqualityTestWithIgnore (fmap T.unpack ignoreList)
  let diff = getGroupedDiffBy equalityTest goldenLines outputLines
  if all isBoth diff
    then Nothing
    else Just $ "Output differs:" <> ppDiff diff
  where
    isBoth :: Diff a -> Bool
    isBoth (Both a b) = True
    isBoth _          = False

diffEqualityTestWithIgnore :: [String] -> String -> String -> Bool
diffEqualityTestWithIgnore ignoreList x y =
  x == y || any (bothContain x y) ignoreList
  where
    bothContain :: String -> String -> String -> Bool
    bothContain x y e = e `isInfixOf` x && e `isInfixOf` y

cleanupGoldenTestOutput :: Bool -> FilePath -> TestTree -> TestTree
cleanupGoldenTestOutput isFile testFile test =
  withResource (return ()) (const handledCleanup) (const test)
  where
    cleanup = if isFile
      then removeFile testFile
      else removeDirectoryRecursive testFile

    handledCleanup = cleanup `catch` (\e ->
      if isDoesNotExistError e
        then return ()
        else throwIO e)

--------------------------------------------------------------------------------
-- Single file golden tests

goldenFileTest :: TestName -> IO () -> [Text] -> FilePath -> FilePath -> TestTree
goldenFileTest testName generateOutput ignoreList goldenFile outputFile = testWithCleanup
  where
  readGolden = TIO.readFile goldenFile
  readOutput = do generateOutput; TIO.readFile outputFile
  updateGolden = TIO.writeFile goldenFile
  diffCommand  = diffTextCommand ignoreList
  test = goldenTest testName readGolden readOutput diffCommand updateGolden
  testWithCleanup = cleanupGoldenTestOutput True outputFile test

diffTextCommand :: [Text] -> Text -> Text -> IO (Maybe String)
diffTextCommand ignoreList golden output = return $ diffText ignoreList golden output

--------------------------------------------------------------------------------
-- Directory golden tests

goldenDirectoryTest :: TestName -> IO () -> [Text] -> FilePath -> FilePath -> TestTree
goldenDirectoryTest testName generateOutput ignoreList goldenDir outputDir = testWithCleanup
  where
    readGolden = readDirectory goldenDir
    readOutput = do generateOutput; readDirectory outputDir
    updateGolden = updateGoldenDirectory goldenDir
    diffCommand  = compareDirectoryContents ignoreList
    test = goldenTest testName readGolden readOutput diffCommand updateGolden
    testWithCleanup = cleanupGoldenTestOutput False outputDir test

readDirectory :: FilePath -> IO (Maybe (Map FilePath Text))
readDirectory directory = do
  -- Try listing the files in the directory
  maybeFiles <- (Just <$> listDirectory directory) `catch` (\e ->
      if isDoesNotExistError e
        then return Nothing
        else throwIO e)

  case maybeFiles of
    Nothing -> return Nothing
    Just files -> do
      let filePaths = map (directory </>) files
      contents <- mapM TIO.readFile filePaths
      return $ Just $ Map.fromList $ zip files contents

compareDirectoryContents :: [Text]
                         -> Maybe (Map FilePath Text)
                         -> Maybe (Map FilePath Text)
                         -> IO (Maybe String)
compareDirectoryContents _ _ Nothing =
  return $ Just "No output directory was created by the test"
compareDirectoryContents _ Nothing _ =
  return $ Just "No golden directory was found for the test. Accept the test to create it."
compareDirectoryContents ignoreList (Just goldenContents) (Just outputContents) = do
  let (sharedFiles, missingFiles, extraFiles) =
        getContentsDiff goldenContents outputContents

  if Map.size sharedFiles /= Map.size goldenContents then do
    let errorMessage = "Contents of output directory did not match golden directory\n"
    let errorMessage1 = errorMessage <>
          if Map.size missingFiles /= 0 then
            "  Missing output files: " <> show (Map.keys missingFiles) <> "\n"
          else ""

    let errorMessage2 = errorMessage1 <>
          if Map.size extraFiles /= 0 then
            "  Extra output files:   " <> show (Map.keys extraFiles) <> "\n"
          else ""

    return $ Just errorMessage2
  else do
    let diffs = map getDiff (Map.toAscList sharedFiles)
    return $ intercalate "\n" <$> sequence diffs
    where
      getDiff :: (FilePath, (Text, Text)) -> Maybe String
      getDiff (file, (goldenText, outputText)) =
        let diff = diffText ignoreList goldenText outputText in
        fmap (\x -> "File '" <> file <> "' " <> x) diff

getContentsDiff :: Map FilePath Text
                -> Map FilePath Text
                -> ( Map FilePath (Text, Text)
                   , Map FilePath Text
                   , Map FilePath Text
                   )
getContentsDiff goldenContents outputContents = (sharedFiles, missingFiles, extraFiles)
  where
  missingFiles = goldenContents \\ outputContents
  extraFiles   = outputContents \\ goldenContents
  sharedFiles  = Map.intersectionWith (,) goldenContents outputContents

updateGoldenDirectory :: FilePath -> Maybe (Map FilePath Text) -> IO ()
updateGoldenDirectory goldenDirectory Nothing            = return ()
updateGoldenDirectory goldenDirectory (Just newContents) = do
  maybeOldContents <- readDirectory goldenDirectory
  case maybeOldContents of
    Nothing -> do
      -- If the Golden directory is missing then create it and
      -- add its contents.
      createDirectory goldenDirectory
      writeNewContents

    Just oldContents -> do
      let (sharedFiles, missingFiles, extraFiles) =
            getContentsDiff newContents oldContents

      if Map.size missingFiles /= 0 || Map.size extraFiles /= 0 then do
        -- If the files themselves are not equal then add/remove the
        -- appropriate files but *do not* update the shared files contents
        -- as directory tests use a 2 phase acceptance model.
        let extraFilePaths = (goldenDirectory </>) <$> Map.keys extraFiles
        mapM_ removeFile extraFilePaths

        let missingFilePaths = first (goldenDirectory </>) <$> Map.toAscList missingFiles
        mapM_ (uncurry TIO.writeFile) missingFilePaths

      -- If the files themselves are equal
      else writeNewContents

  where
    writeNewContents :: IO ()
    writeNewContents = do
      let pairs = fmap (first (goldenDirectory </>)) (Map.toAscList newContents)
      mapM_ (uncurry TIO.writeFile) pairs