module General.Extra.File where

-- import Codec.Text.Detect (detectEncodingName)
import Control.Exception (IOException, throwIO, try)
import Control.Monad (filterM, forM, when)
-- import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.DList (DList)
import Data.DList qualified as DList
import Data.Text (Text)
import Data.Text.IO qualified as Text
import System.Directory
  ( copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    getDirectoryContents,
    listDirectory,
    removeFile,
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (ReadMode), withFile)

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

-- | Copy files, recursively.
copyRecursively :: FilePath -> FilePath -> IO [FilePath]
copyRecursively src dst = do
  whenM (not <$> doesPathExist src) $
    throwIO (userError $ "test source file '" <> src <> "' does not exist")

  isDirectory <- doesDirectoryExist src
  if isDirectory
    then do
      createDirectory dst
      content <- getDirectoryContents src
      let xs = filter (`notElem` ([".", ".."] :: [FilePath])) content
      copiedFiles <- forM xs $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        copyRecursively srcPath dstPath
      return $ concat copiedFiles
    else do
      copyFile src dst
      return [dst]
  where
    whenM s r = s >>= flip when r
