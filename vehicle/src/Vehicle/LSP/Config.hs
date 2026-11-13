{-# LANGUAGE OverloadedRecordDot #-}

module Vehicle.LSP.Config
  ( packageName,
    Config (..),
    defaultConfig,
    parseConfig,
  )
where

import Data.Aeson.Types (FromJSON (..), KeyValue (..), Parser, Result (..), ToJSON (..), Value, fromJSON, object, withObject, (.!=), (.:?))
import Data.Default (Default (..))
import Data.Text (Text)
import Data.Text qualified as T

--------------------------------------------------------------------------------
-- Language-Server Package Name
--------------------------------------------------------------------------------

packageName :: Text
packageName = T.pack "vehicle-lsp"

--------------------------------------------------------------------------------
-- Language-Server Configuration
--------------------------------------------------------------------------------

newtype Config = Config
  { maxNumberOfProblems :: Int
  }

defaultConfig :: Config
defaultConfig =
  Config
    { maxNumberOfProblems = 100
    }

instance Default Config where
  def = defaultConfig

instance FromJSON Config where
  parseJSON :: Value -> Parser Config
  parseJSON = withObject "Config" $ \l -> do
    maxNumberOfProblems <- l .:? "maxNumberOfProblems" .!= (def @Config).maxNumberOfProblems
    pure Config {..}

instance ToJSON Config where
  toJSON :: Config -> Value
  toJSON Config {..} =
    object
      [ "maxNumberOfProblems" .= maxNumberOfProblems
      ]

parseConfig :: Config -> Value -> Either Text Config
parseConfig _oldConfig newConfigRaw =
  case fromJSON newConfigRaw of
    Error errorMessage -> Left (T.pack errorMessage)
    Success newConfig -> Right newConfig
