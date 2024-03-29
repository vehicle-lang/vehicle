module Test.Tasty.Golden.Executable.TestSpec.FilePattern
  ( FilePattern,
    glob,
    match,
    addExtension,
    asLiteral,
  )
where

import Control.Arrow (Arrow ((&&&)))
import Data.Aeson (Value)
import Data.Aeson.Types (FromJSON, Parser, ToJSON (..), typeMismatch)
import Data.Aeson.Types qualified as Value
import Data.Data (Typeable)
import Data.Function (on)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.String (IsString (..))
import Data.Text qualified as Text (unpack)
import System.FilePath qualified as FilePath (addExtension, normalise)
import System.FilePath.Glob (CompOptions (..))
import System.FilePath.Glob qualified as Glob
import Test.Tasty.Options (safeRead)

-- | Type of file patterns.
--
--   Consists of a pair of the original string and the parsed glob pattern,
--   so that we can output the original string in `show` and `toJSON`.
data FilePattern = FilePattern
  { patternString :: FilePath,
    patternExtensions :: Seq String,
    pattern :: Glob.Pattern
  }
  deriving (Typeable)

-- | Find matches for a list of file patterns in the given directory.
glob :: [FilePattern] -> FilePath -> IO [FilePath]
glob pats dir = concat <$> Glob.globDir (pattern <$> pats) dir

-- | Test if a file matches a file pattern.
match :: FilePattern -> FilePath -> Bool
match pat = Glob.match (pattern pat)

-- | Add an extension to a file pattern.
addExtension :: FilePattern -> FilePath -> FilePattern
addExtension (FilePattern patternString exts _) ext =
  FilePattern patternString (exts |> ext) $
    Glob.compileWith globCompOptions (FilePath.addExtension patternString ext)

-- | Return the literal 'FilePath', if the 'FilePattern' is a literal path,
--   i.e., it has no wildcards, character ranges, etc.
asLiteral :: FilePattern -> Maybe FilePath
asLiteral (FilePattern patternString _ pattern)
  | Glob.isLiteral pattern = Just (FilePath.normalise patternString)
  | otherwise = Nothing

-- | Parse a file pattern.
readEither :: String -> Either String FilePattern
readEither patternString =
  FilePattern patternString Seq.empty
    <$> Glob.tryCompileWith globCompOptions (FilePath.normalise patternString)

globCompOptions :: Glob.CompOptions
globCompOptions =
  CompOptions
    { characterClasses = False,
      characterRanges = False,
      numberRanges = False,
      wildcards = True,
      recursiveWildcards = True,
      pathSepInRanges = False,
      errorRecovery = False
    }

instance Eq FilePattern where
  (==) :: FilePattern -> FilePattern -> Bool
  (==) = (==) `on` (patternString &&& patternExtensions)

instance Ord FilePattern where
  compare :: FilePattern -> FilePattern -> Ordering
  compare = compare `on` (patternString &&& patternExtensions)

instance Show FilePattern where
  show :: FilePattern -> String
  show FilePattern {..} =
    foldr (flip FilePath.addExtension) patternString patternExtensions

instance Read FilePattern where
  readsPrec :: Int -> ReadS FilePattern
  readsPrec _prec = either (const []) (\pat -> [(pat, "")]) . readEither

instance IsString FilePattern where
  fromString :: String -> FilePattern
  fromString input = case safeRead input of
    Just filePattern -> filePattern
    Nothing -> error $ "Could not parse FilePattern '" <> input <> "'"

instance FromJSON FilePattern where
  parseJSON :: Value -> Parser FilePattern
  parseJSON (Value.String patternText) = either fail return $ readEither (Text.unpack patternText)
  parseJSON v = typeMismatch "String" v

instance ToJSON FilePattern where
  toJSON :: FilePattern -> Value
  toJSON = toJSON . patternString
