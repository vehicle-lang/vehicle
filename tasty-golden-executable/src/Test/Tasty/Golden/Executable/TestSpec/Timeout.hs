module Test.Tasty.Golden.Executable.TestSpec.Timeout
  ( Timeout,
    isTimeout,
    toSomeOption,
  )
where

import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value, typeMismatch)
import Data.Aeson.Types qualified as Value (Value (..))
import Data.Data (Typeable)
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Ord (Down (Down))
import Data.Text qualified as Text
import General.Extra.Option (SomeOption (AdjustOption))
import Test.Tasty qualified as Tasty (Timeout (NoTimeout, Timeout))
import Test.Tasty.Options (IsOption (..))

newtype Timeout = Timeout {toTastyTimeout :: Tasty.Timeout}
  deriving (Show, Typeable)

instance Semigroup Timeout where
  (<>) :: Timeout -> Timeout -> Timeout
  t1 <> t2 = t1 `min` t2

instance Monoid Timeout where
  mempty :: Timeout
  mempty = Timeout Tasty.NoTimeout

-- | As 'SomeOption'.
toSomeOption :: Timeout -> SomeOption
toSomeOption (Timeout tt1) = AdjustOption adjustTimeout
  where
    adjustTimeout :: Tasty.Timeout -> Tasty.Timeout
    adjustTimeout tt2 = if compareTastyTimeout tt1 tt2 == LT then tt1 else tt2

-- | Check whether a timeout represents an finite timeout.
isTimeout :: Timeout -> Bool
isTimeout = isJust . getTimeoutMS

instance Eq Timeout where
  (==) :: Timeout -> Timeout -> Bool
  (==) = (==) `on` getTimeoutMS

instance Ord Timeout where
  compare :: Timeout -> Timeout -> Ordering
  compare = compareTastyTimeout `on` toTastyTimeout

instance FromJSON Timeout where
  parseJSON :: Value -> Parser Timeout
  parseJSON (Value.String timeoutText) =
    let timeoutString = Text.unpack timeoutText
     in case parseValue timeoutString of
          Nothing ->
            fail $
              unlines
                [ "Could not parse value of 'timeout'.",
                  "Expected a number, optionally followed by a suffix (ms, s, m, h).",
                  "Found: " <> timeoutString
                ]
          Just tastyTimeout -> return $ Timeout tastyTimeout
  parseJSON value = typeMismatch "String" value

instance ToJSON Timeout where
  toJSON :: Timeout -> Value
  toJSON (Timeout (Tasty.Timeout _ms timeoutString)) = Value.String (Text.pack timeoutString)
  toJSON (Timeout Tasty.NoTimeout) = Value.Null

-- | Get the timeout in milliseconds.
getTimeoutMS :: Timeout -> Maybe Integer
getTimeoutMS = getTastyTimeoutMS . toTastyTimeout

-- | Compare two values of type 'Tasty.Timeout'.
compareTastyTimeout :: Tasty.Timeout -> Tasty.Timeout -> Ordering
compareTastyTimeout = compare `on` (Down . fmap Down . getTastyTimeoutMS)

-- | Get '()' or the timeout in milliseconds for a value of type 'Tasty.Timeout'.
getTastyTimeoutMS :: Tasty.Timeout -> Maybe Integer
getTastyTimeoutMS (Tasty.Timeout ms _) = Just ms
getTastyTimeoutMS _ = Nothing
