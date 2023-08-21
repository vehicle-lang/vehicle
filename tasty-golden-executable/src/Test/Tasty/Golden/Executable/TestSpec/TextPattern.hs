module Test.Tasty.Golden.Executable.TestSpec.TextPattern
  ( TextPattern,
    strikeOut,
  )
where

import Data.Aeson (Value)
import Data.Aeson.Types (FromJSON, Parser, ToJSON (..), typeMismatch)
import Data.Aeson.Types qualified as Value
import Data.Array qualified as Array ((!))
import Data.Data (Typeable)
import Data.Function (on)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Regex.TDFA qualified as Regex
import Text.Regex.TDFA.Text (Regex)
import Text.Regex.TDFA.Text qualified as Regex

-- | Type of text patterns.
--
--   Consists of a pair of the original string and the parsed regular expression,
--   so that we can output the original string in `show` and `toJSON`.
data TextPattern = TextPattern
  { patternString :: Text,
    pattern :: Regex
  }
  deriving (Typeable)

readEither :: String -> Either String TextPattern
readEither = readTextEither . Text.pack

readTextEither :: Text -> Either String TextPattern
readTextEither regexText =
  TextPattern regexText <$> Regex.compile Regex.defaultCompOpt Regex.defaultExecOpt regexText

-- | Strike out matches for the given 'TextPattern'.
strikeOut :: TextPattern -> Text -> Text
strikeOut TextPattern {..} = strikeOutTop pattern
  where
    strikeOutTop :: Regex -> Text -> Text
    strikeOutTop re txt = strikeOutAcc (Regex.matchAll re txt) txt []

    strikeOutAcc :: [Regex.MatchArray] -> Text -> [Text] -> Text
    strikeOutAcc [] txt acc = Text.concat (reverse (txt : acc))
    strikeOutAcc (match : matches) txt acc = strikeOutAcc matches rest newAcc
      where
        (matchOffset, matchLength) = match Array.! 0
        (beforeMatch, matchAndAfterMatch) = Text.splitAt matchOffset txt
        (_matchText, rest) = Text.splitAt matchLength matchAndAfterMatch
        newAcc = "[REDACTED]" : beforeMatch : acc

instance Eq TextPattern where
  (==) :: TextPattern -> TextPattern -> Bool
  (==) = (==) `on` patternString

instance Ord TextPattern where
  compare :: TextPattern -> TextPattern -> Ordering
  compare = compare `on` patternString

instance Show TextPattern where
  show :: TextPattern -> String
  show TextPattern {..} = Text.unpack patternString

instance Read TextPattern where
  readsPrec :: Int -> ReadS TextPattern
  readsPrec _prec = either (const []) (\pat -> [(pat, "")]) . readEither

instance FromJSON TextPattern where
  parseJSON :: Value -> Parser TextPattern
  parseJSON (Value.String patternText) = either fail return $ readTextEither patternText
  parseJSON v = typeMismatch "String" v

instance ToJSON TextPattern where
  toJSON :: TextPattern -> Value
  toJSON = toJSON . patternString
