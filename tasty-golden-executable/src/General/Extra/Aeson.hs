module General.Extra.Aeson where

import Data.Aeson.Types (Parser, Value (..), typeMismatch)
import Data.Foldable (Foldable (toList))
import Data.Text (Text)

-- | Parse values from either a String or an Array of Strings.
fromStringOrStringArray :: (Text -> Parser a) -> Value -> Parser [a]
fromStringOrStringArray textParser value = case value of
  String text -> (: []) <$> textParser text
  Array items -> traverse valueParser (toList items)
  _ -> typeMismatch "String or Array" value
  where
    valueParser (String text) = textParser text
    valueParser v = typeMismatch "String" v
