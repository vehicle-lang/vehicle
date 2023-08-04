module Test.Tasty.Golden.Executable.TestSpec.External
  ( External (..),
  )
where

import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value, typeMismatch)
import Data.Aeson.Types qualified as Value (Value (..))
import Data.Data (Typeable)
import Data.Text qualified as Text

-- | The name of an external program.
newtype External = External {programNames :: [FilePath]}
  deriving (Eq, Ord, Show, Typeable)

instance FromJSON External where
  parseJSON :: Value -> Parser External
  parseJSON (Value.String name) = return $ External [Text.unpack name]
  parseJSON value = typeMismatch "String" value

instance ToJSON External where
  toJSON :: External -> Value
  toJSON = toJSON . programNames
