module Vehicle.Test.Golden.TestSpec.FilePattern
  ( IsFilePattern (..),
    FilePattern,
    GoldenFilePattern,
  )
where

import Data.Aeson (Value)
import Data.Aeson.Types (FromJSON, Parser, ToJSON (..), typeMismatch)
import Data.Aeson.Types qualified as Value
import Data.Data (Typeable)
import Data.Function (on)
import Data.String (IsString (fromString))
import Data.Text as Text (unpack)
import System.FilePath ((<.>))
import System.FilePath qualified as FilePath (isRelative)
import System.FilePath.Glob (CompOptions (..))
import System.FilePath.Glob qualified as Glob

class IsFilePattern filePattern where
  isRelative :: filePattern -> Bool
  glob :: [filePattern] -> FilePath -> IO [FilePath]
  match :: filePattern -> FilePath -> Bool
  readEither :: String -> Either String filePattern

instance IsFilePattern FilePattern where
  isRelative :: FilePattern -> Bool
  isRelative pat = FilePath.isRelative (filePatternString pat)

  glob :: [FilePattern] -> FilePath -> IO [FilePath]
  glob pats dir = concat <$> Glob.globDir (filePattern <$> pats) dir

  match :: FilePattern -> FilePath -> Bool
  match pat = Glob.match (filePattern pat)

  readEither :: String -> Either String FilePattern
  readEither = parseFilePattern

-- | Type of file patterns.
--
--   Consists of a pair of the original string and the parsed glob pattern,
--   so that we can output the original string in `show` and `toJSON`.
data FilePattern = FilePattern
  { filePatternString :: String,
    filePattern :: Glob.Pattern
  }
  deriving (Typeable)

instance Eq FilePattern where
  (==) :: FilePattern -> FilePattern -> Bool
  (==) = (==) `on` filePatternString

instance Ord FilePattern where
  compare :: FilePattern -> FilePattern -> Ordering
  compare = compare `on` filePatternString

instance IsString FilePattern where
  fromString :: String -> FilePattern
  fromString str = case parseFilePattern str of
    Left err -> error err
    Right fp -> fp

parseFilePattern :: String -> Either String FilePattern
parseFilePattern patternString = do
  FilePattern patternString <$> eitherGlobPattern
  where
    eitherGlobPattern = Glob.tryCompileWith compOptions patternString
    compOptions =
      CompOptions
        { characterClasses = False,
          characterRanges = False,
          numberRanges = False,
          wildcards = True,
          recursiveWildcards = True,
          pathSepInRanges = False,
          errorRecovery = False
        }

instance Show FilePattern where
  show :: FilePattern -> String
  show FilePattern {..} = show filePatternString

instance FromJSON FilePattern where
  parseJSON :: Value -> Parser FilePattern
  parseJSON (Value.String patternText) =
    either fail return $ parseFilePattern (Text.unpack patternText)
  parseJSON v = typeMismatch "String" v

instance ToJSON FilePattern where
  toJSON :: FilePattern -> Value
  toJSON = toJSON . filePatternString

instance IsFilePattern GoldenFilePattern where
  isRelative :: GoldenFilePattern -> Bool
  isRelative pat = FilePath.isRelative (goldenFilePatternString pat)

  glob :: [GoldenFilePattern] -> FilePath -> IO [FilePath]
  glob pats = glob (goldenFilePattern <$> pats)

  match :: GoldenFilePattern -> FilePath -> Bool
  match pat = match (goldenFilePattern pat)

  readEither :: String -> Either String GoldenFilePattern
  readEither patternString = do
    filePattern <- readEither (patternString <.> ".golden")
    return $ GoldenFilePattern patternString filePattern

data GoldenFilePattern = GoldenFilePattern
  { goldenFilePatternString :: String,
    goldenFilePattern :: FilePattern
  }
  deriving (Eq, Ord, Typeable)

instance Show GoldenFilePattern where
  show :: GoldenFilePattern -> String
  show GoldenFilePattern {..} = show goldenFilePatternString

instance FromJSON GoldenFilePattern where
  parseJSON :: Value -> Parser GoldenFilePattern
  parseJSON (Value.String patternText) = do
    let patternString = Text.unpack patternText
    filePattern <- either fail return $ parseFilePattern (patternString <.> ".golden")
    return $ GoldenFilePattern patternString filePattern
  parseJSON v = typeMismatch "String" v

instance ToJSON GoldenFilePattern where
  toJSON :: GoldenFilePattern -> Value
  toJSON = toJSON . goldenFilePatternString
