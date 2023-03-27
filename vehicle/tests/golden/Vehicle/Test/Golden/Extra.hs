module Vehicle.Test.Golden.Extra where

import Control.Exception (IOException, try)
import Control.Monad (filterM, when)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removeFile,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (ReadMode), withFile)
import Test.Tasty (TestTree, localOption)
import Test.Tasty.Options (IsOption)

-- | Existential type of test options.
data SomeOption = forall v. (IsOption v) => SomeOption v

-- | Apply a test option.
someLocalOption :: SomeOption -> TestTree -> TestTree
someLocalOption (SomeOption option) = localOption option

-- | Apply a list of test options.
someLocalOptions :: [SomeOption] -> TestTree -> TestTree
someLocalOptions someOptions testTree = foldr someLocalOption testTree someOptions

-- | Write a file, but only if the contents would change.
--
--   Adapted from shake (BSD-3-Clause):
--   https://hackage.haskell.org/package/shake-0.19.7/docs/src/
--   Development.Shake.Internal.Derived.html#writeFileChanged
writeFileChanged :: FilePath -> Text -> IO ()
writeFileChanged name x = do
  createDirectoryRecursive $ takeDirectory name
  nameExists <- doesFileExist name
  if not nameExists
    then Text.writeFile name x
    else do
      -- Cannot use ByteString here, since it has different line handling
      -- semantics on Windows
      nameOpenSuccess <- withFile name ReadMode $ \h -> do
        src <- Text.hGetContents h
        pure $! src /= x
      when nameOpenSuccess $ do
        removeFile name -- symlink safety
        Text.writeFile name x

-- | Like @createDirectoryIfMissing True@ but faster, as it avoids
--   any work in the common case the directory already exists.
--
--   Taken from shake (BSD-3-Clause):
--   https://hackage.haskell.org/package/shake-0.19.7/docs/src/
--   General.Extra.html#createDirectoryRecursive
createDirectoryRecursive :: FilePath -> IO ()
createDirectoryRecursive dir = do
  x <- try @IOException $ doesDirectoryExist dir
  when (x /= Right True) $ createDirectoryIfMissing True dir

-- | If the boolean condition holds, return `Just a`.
--   Otherwise, return `Nothing`.
boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe just
  | just = Just
  | otherwise = const Nothing

-- | List all files in a directory, recursively.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive directoryPath = do
  directoryPathExists <- doesDirectoryExist directoryPath
  if not directoryPathExists
    then return []
    else do
      DList.toList <$> listFilesRecursiveDList directoryPath
  where
    listFilesRecursiveDList :: FilePath -> IO (DList FilePath)
    listFilesRecursiveDList directory = do
      entryNames <- listDirectory directory
      let entryPaths = (directory </>) <$> entryNames
      filePaths <- DList.fromList <$> filterM doesFileExist entryPaths
      subdirectory <- DList.fromList <$> filterM doesDirectoryExist entryPaths
      subdirectoryFiles <- foldMap listFilesRecursiveDList subdirectory
      return $ filePaths <> subdirectoryFiles

-- | Find the duplicate elements in a list:
duplicates :: (Eq a, Hashable a) => [a] -> [a]
duplicates = duplicatesAcc HashSet.empty
  where
    duplicatesAcc _seen [] = []
    duplicatesAcc seen (x : xs)
      | x `HashSet.member` seen = x : duplicatesAcc seen xs
      | otherwise = duplicatesAcc (HashSet.insert x seen) xs
