module Vehicle.Test.Utils.Golden
  ( goldenDirectoryTest
  , goldenFileTest
  , noException
  , omitFilePaths
  , cleanupOutput
  ) where

import Test.Tasty
import Test.Tasty.Golden.Advanced
import Data.Text ( Text )
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Map (Map, (\\))
import Data.Map qualified as Map (toAscList, fromList, keysSet, size, lookup, intersectionWith, keys, null)
import Data.Set qualified as Set
import System.Directory (listDirectory, removeFile, createDirectory, removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath (takeFileName, (</>), takeDirectory)
import Data.List (intercalate, isInfixOf)
import System.IO.Error
import Control.Exception (catch, throwIO)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Maybe (catMaybes, listToMaybe)

import Data.Algorithm.Diff (Diff, PolyDiff(..), getGroupedDiffBy)
import Data.Algorithm.DiffOutput (ppDiff)

import Vehicle.Test.Utils.FilePath (removeFilePaths)

--------------------------------------------------------------------------------
-- General utilities

-- | Exceptions for diffing a list of text. At the moment this is implemented as
-- a function that is applied upon the failure of two lines, upon which the
-- diff is then reperformed.
type DiffException = String -> String -> Bool

noException :: DiffException
noException _ _ = False

omitFilePaths :: DiffException
omitFilePaths x y = removeFilePaths x == removeFilePaths y

diffText :: DiffException -> Text -> Text -> Maybe String
diffText failTransformation golden output = do
  let goldenLines  = map T.unpack $ T.lines golden
  let outputLines  = map T.unpack $ T.lines output
  let equalityTest = diffEqualityTestWithIgnore failTransformation
  let diff = getGroupedDiffBy equalityTest goldenLines outputLines
  if all isBoth diff
    then Nothing
    else Just $ "Output differs:" <> ppDiff diff
  where
    isBoth :: Diff a -> Bool
    isBoth (Both a b) = True
    isBoth _          = False

diffEqualityTestWithIgnore :: DiffException -> String -> String -> Bool
diffEqualityTestWithIgnore exceptionTest golden output =
  golden == output || exceptionTest golden output

cleanupTestOutput :: Bool -> FilePath -> TestTree -> TestTree
cleanupTestOutput isFile testFile test =
  withResource (return ()) (const (cleanupOutput isFile testFile)) (const test)

cleanupOutput :: Bool -> FilePath -> IO ()
cleanupOutput isFile testFile =
  cleanup `catch` (\e ->
      if isDoesNotExistError e
        then return ()
        else throwIO e)
  where
    cleanup = if isFile
      then removeFile testFile
      else removeDirectoryRecursive testFile

--------------------------------------------------------------------------------
-- Single file golden tests

goldenFileTest :: TestName -> IO () -> DiffException -> FilePath -> FilePath -> TestTree
goldenFileTest testName generateOutput diffException goldenFile outputFile = testWithCleanup
  where
  readGolden = TIO.readFile goldenFile
  readOutput = do generateOutput; TIO.readFile outputFile
  updateGolden = TIO.writeFile goldenFile
  diffCommand  = diffTextCommand diffException
  test = goldenTest testName readGolden readOutput diffCommand updateGolden
  testWithCleanup = cleanupTestOutput True outputFile test

diffTextCommand :: DiffException -> Text -> Text -> IO (Maybe String)
diffTextCommand diffException golden output = return $ diffText diffException golden output

--------------------------------------------------------------------------------
-- Directory golden tests

goldenDirectoryTest :: TestName -> IO () -> DiffException -> FilePath -> FilePath -> TestTree
goldenDirectoryTest testName generateOutput diffException goldenDir outputDir = testWithCleanup
  where
  readGolden = readDirectory goldenDir
  readOutput = do generateOutput; readDirectory outputDir
  updateGolden = updateGoldenDirectory goldenDir
  diffCommand  = compareDirectoryContents diffException
  test = goldenTest testName readGolden readOutput diffCommand updateGolden
  testWithCleanup = cleanupTestOutput False outputDir test

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

compareDirectoryContents :: DiffException
                         -> Maybe (Map FilePath Text)
                         -> Maybe (Map FilePath Text)
                         -> IO (Maybe String)
compareDirectoryContents _ _ Nothing =
  return $ Just "No output directory was created by the test"
compareDirectoryContents _ Nothing _ =
  return $ Just "No golden directory was found for the test. Accept the test to create it."
compareDirectoryContents diffException (Just goldenContents) (Just outputContents) = do
  let (sharedFiles, missingFiles, extraFiles) =
        getContentsDiff goldenContents outputContents

  if not (Map.null missingFiles) || not (Map.null extraFiles) then do
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
    let diff = intercalate "\n" (catMaybes diffs)
    return (if null diff then Nothing else Just diff)
    where
      getDiff :: (FilePath, (Text, Text)) -> Maybe String
      getDiff (file, (goldenText, outputText)) =
        let diff = diffText diffException goldenText outputText in
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