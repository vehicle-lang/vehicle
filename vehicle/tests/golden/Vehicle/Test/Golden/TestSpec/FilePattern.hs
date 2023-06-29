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
import System.FilePath.Glob (CompOptions (..))
import System.FilePath.Glob qualified as Glob

class (Read filePattern, Show filePattern) => IsFilePattern filePattern where
  glob :: [filePattern] -> FilePath -> IO [FilePath]
  match :: filePattern -> FilePath -> Bool

instance IsFilePattern FilePattern where
  glob :: [FilePattern] -> FilePath -> IO [FilePath]
  glob pats dir = concat <$> Glob.globDir (filePattern <$> pats) dir

  match :: FilePattern -> FilePath -> Bool
  match pat = Glob.match (filePattern pat)

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
  fromString = read

instance Show FilePattern where
  show :: FilePattern -> String
  show FilePattern {..} = filePatternString

instance Read FilePattern where
  readsPrec :: Int -> ReadS FilePattern
  readsPrec _prec str = case parseFilePattern str of
    Left _err -> []
    Right pat -> [(pat, "")]

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

instance FromJSON FilePattern where
  parseJSON :: Value -> Parser FilePattern
  parseJSON (Value.String patternText) =
    either fail return $ parseFilePattern (Text.unpack patternText)
  parseJSON v = typeMismatch "String" v

instance ToJSON FilePattern where
  toJSON :: FilePattern -> Value
  toJSON = toJSON . filePatternString

instance IsFilePattern GoldenFilePattern where
  glob :: [GoldenFilePattern] -> FilePath -> IO [FilePath]
  glob pats = glob (goldenFilePattern <$> pats)

  match :: GoldenFilePattern -> FilePath -> Bool
  match pat = match (goldenFilePattern pat)

data GoldenFilePattern = GoldenFilePattern
  { goldenFilePatternString :: String,
    goldenFilePattern :: FilePattern
  }
  deriving (Eq, Ord, Typeable)

instance Show GoldenFilePattern where
  show :: GoldenFilePattern -> String
  show GoldenFilePattern {..} = goldenFilePatternString

instance Read GoldenFilePattern where
  readsPrec :: Int -> ReadS GoldenFilePattern
  readsPrec _prec str = case parseFilePattern (str <.> ".golden") of
    Left _err -> []
    Right pat -> [(GoldenFilePattern str pat, "")]

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
