module Test.Tasty.Golden.Executable.TestSpecs
  ( TestSpecs,
    readTestSpecsFile,
    writeTestSpecsFile,
    makeTestTreeFromFile,
    makeTestTreeFromDirectoryRecursive,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (filterM, unless)
import Control.Monad.Writer (Any (..), MonadWriter (..), Writer, runWriter)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePrettyToTextBuilder', keyOrder)
import Data.Aeson.Encode.Pretty qualified as Indent (Indent (..))
import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder qualified as Builder
import General.Extra.Option (SomeOption, someLocalOptions)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Test.Tasty (testGroup)
import Test.Tasty.Golden.Executable.TestSpec (TestSpec (..), isEnabled)
import Test.Tasty.Providers (TestName, TestTree, singleTest)
import Text.Printf (printf)

newtype TestSpecs = TestSpecs {unTestSpecs :: NonEmpty TestSpec}

-- | Read 'TestSpecs' from a file.
readTestSpecsFile :: FilePath -> IO TestSpecs
readTestSpecsFile testSpecFile = do
  let testDirectory = takeDirectory testSpecFile
  let addTestSpecDirectory testSpec = testSpec {testSpecDirectory = testDirectory}
  eitherDecodeFileStrict' testSpecFile >>= \case
    Left parseError -> fail $ printf "Could not parse %s: %s" testSpecFile parseError
    Right testSpecs -> do
      let duplicates = duplicateTestNames testSpecs
      unless (null duplicates) $
        fail $
          printf "Duplicate names in %s: %s" testSpecFile (intercalate ", " duplicates)
      return $ TestSpecs (addTestSpecDirectory <$> unTestSpecs testSpecs)

-- | Read a test specification and return a TestTree.
makeTestTreeFromFile :: [SomeOption] -> FilePath -> IO [TestTree]
makeTestTreeFromFile testOptions testSpecFile = do
  TestSpecs testSpecs <- readTestSpecsFile testSpecFile
  return
    [ someLocalOptions testOptions (singleTest (testSpecName testSpec) testSpec)
      | testSpec <- NonEmpty.toList testSpecs,
        isEnabled testSpec
    ]

-- | Create a test tree from all test specifications in a directory, recursively.
makeTestTreeFromDirectoryRecursive :: [SomeOption] -> TestName -> FilePath -> IO TestTree
makeTestTreeFromDirectoryRecursive testOptions testGroupLabel testDirectory = do
  -- List all paths in `testDirectory`
  testDirectoryEntries <- listDirectory testDirectory

  -- Construct test trees for each test.json file in the current directory:
  testTreesFromHere <-
    -- Filter directory entries to only test specifications
    filterM (isTestSpecFile . (testDirectory </>)) testDirectoryEntries
      -- Make test trees
      >>= traverse
        ( \testSpecFileName ->
            makeTestTreeFromFile testOptions (testDirectory </> testSpecFileName)
        )
      <&> concat

  -- Construct test trees for all subdirectories:
  testTreesFromFurther <-
    -- Filter directory entries to only test specifications:
    filterM (doesDirectoryExist . (testDirectory </>)) testDirectoryEntries
      -- Make test trees for each subdirectory:
      >>= traverse
        ( \subDirectoryName ->
            let testSubDirectory = testDirectory </> subDirectoryName
             in makeTestTreeFromDirectoryRecursive testOptions subDirectoryName testSubDirectory
        )

  -- Combine all test trees:
  let result = testGroup testGroupLabel (testTreesFromHere <> testTreesFromFurther)

  return result

-- | Test whether a path refers to an existing test specification file.
isTestSpecFile :: FilePath -> IO Bool
isTestSpecFile path = (takeFileName path == "test.json" &&) <$> doesFileExist path

-- | Find all duplicate test names in a 'TestSpecs'.
duplicateTestNames :: TestSpecs -> [TestName]
duplicateTestNames = duplicates Set.empty . map testSpecName . NonEmpty.toList . unTestSpecs
  where
    duplicates :: (Eq a, Ord a) => Set a -> [a] -> [a]
    duplicates _seen [] = []
    duplicates seen (name : names) =
      let rest = duplicates (Set.insert name seen) names
       in if name `Set.member` seen then name : rest else rest

-- | Write 'TestSpecs' to a file.
writeTestSpecsFile :: FilePath -> TestSpecs -> IO ()
writeTestSpecsFile testSpecFile testSpecs = do
  Text.writeFile testSpecFile (encodeTestSpecsPretty testSpecs)

instance Semigroup TestSpecs where
  (<>) :: TestSpecs -> TestSpecs -> TestSpecs
  testSpecs1 <> testSpecs2 =
    foldr consOrReplace testSpecs1 (unTestSpecs testSpecs2)
    where
      consOrReplace :: TestSpec -> TestSpecs -> TestSpecs
      consOrReplace newTestSpec (TestSpecs oldTestSpecs) =
        TestSpecs $ if anyWasReplaced then newTestSpecs else newTestSpec <| oldTestSpecs
        where
          (newTestSpecs, Any anyWasReplaced) = runWriter (traverse (replaceIfSameName newTestSpec) oldTestSpecs)

      replaceIfSameName :: TestSpec -> TestSpec -> Writer Any TestSpec
      replaceIfSameName newTestSpec oldTestSpec =
        if testSpecName newTestSpec == testSpecName oldTestSpec
          then do tell (Any True); return newTestSpec
          else return oldTestSpec

instance FromJSON TestSpecs where
  parseJSON :: Value -> Parser TestSpecs
  parseJSON v =
    TestSpecs <$> parse1 <|> parseN
    where
      parse1 = (:| []) <$> parseJSON v
      parseN = TestSpecs <$> parseJSON v

instance ToJSON TestSpecs where
  toJSON :: TestSpecs -> Value
  toJSON (TestSpecs (testSpec :| [])) = toJSON testSpec
  toJSON testSpecs@(TestSpecs (_ :| _)) = toJSON testSpecs

-- | Encode a TestSpec as JSON using aeson-pretty.
encodeTestSpecsPretty :: TestSpecs -> Text
encodeTestSpecsPretty =
  Lazy.toStrict
    . Builder.toLazyText
    . encodePrettyToTextBuilder'
      defConfig
        { confIndent = Indent.Spaces 2,
          confCompare = keyOrder ["name", "run", "enabled", "needs", "produces", "timeout", "ignore"],
          confTrailingNewline = True
        }
