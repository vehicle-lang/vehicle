module Test.Tasty.Golden.Executable.TestSpecs
  ( TestSpecs,
    readTestSpecsFile,
    writeTestSpecsFile,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (unless)
import Control.Monad.Writer (Any (..), MonadWriter (..), Writer, runWriter)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePrettyToTextBuilder', keyOrder)
import Data.Aeson.Encode.Pretty qualified as Indent (Indent (..))
import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder qualified as Builder
import System.FilePath (takeDirectory)
import Test.Tasty.Golden.Executable.TestSpec (TestSpec (..))
import Test.Tasty.Providers (TestName)
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
  parseJSON v = TestSpecs <$> parse1 <|> parseN
    where
      parse1 = (:| []) <$> parseJSON v
      parseN = parseJSON v

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
