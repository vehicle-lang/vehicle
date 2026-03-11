{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Vehicle.LSP.Internal.Config (
    packageName,
    LspTc,
    runLspTc,
    Config (..),
    defaultConfig,
    parseConfig,
    onConfigChange,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Aeson.Types (FromJSON (..), KeyValue (..), Parser, Result (..), ToJSON (..), Value, fromJSON, object, withObject, (.!=), (.:?))
import Data.Default (Default (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts (oneShot)
import Language.LSP.Server (LanguageContextEnv, MonadLsp (..))

--------------------------------------------------------------------------------
-- Language-Server Package Name
--------------------------------------------------------------------------------

packageName :: Text
packageName = T.pack "vehicle-lsp"

--------------------------------------------------------------------------------
-- Language-Server Type-Checker Monad Stack
--
-- The `LspTc` monad uses a one-shot reader encoding. For details, see the GHC
-- Note [The one-shot state monad trick] or https://github.com/ghc/ghc/blob/
-- ab3ab3e3d489a351e84f4fe681de1731549376a2/compiler/GHC/Utils/Monad.hs#L259
--------------------------------------------------------------------------------

type LspTc :: Type -> Type
newtype LspTc a = LspTc' (LanguageContextEnv Config -> IO a)

pattern LspTc :: forall a. (LanguageContextEnv Config -> IO a) -> LspTc a
pattern LspTc m <- LspTc' m
    where
        LspTc m = LspTc' (oneShot m)

{-# COMPLETE LspTc #-}

runLspTc :: LanguageContextEnv Config -> LspTc a -> IO a
runLspTc lcEnv (LspTc f) = f lcEnv

instance Functor LspTc where
    fmap :: (a -> b) -> LspTc a -> LspTc b
    fmap f (LspTc ma) = LspTc $ \lcEnv -> f <$> ma lcEnv

instance Applicative LspTc where
    pure :: a -> LspTc a
    pure x = LspTc $ \_lcEnv -> pure x

    (<*>) :: LspTc (a -> b) -> LspTc a -> LspTc b
    LspTc mf <*> LspTc ma = LspTc $ \lcEnv -> mf lcEnv <*> ma lcEnv

instance Monad LspTc where
    (>>=) :: LspTc a -> (a -> LspTc b) -> LspTc b
    LspTc ma >>= mf =
        LspTc $ \lcEnv ->
            ma lcEnv
                >>= \a -> let LspTc b = mf a in b lcEnv

instance MonadIO LspTc where
    liftIO :: IO a -> LspTc a
    liftIO m = LspTc $ \_lcEnv -> m

instance MonadUnliftIO LspTc where
    withRunInIO :: ((forall a. LspTc a -> IO a) -> IO b) -> LspTc b
    withRunInIO k = LspTc $ \lcEnv -> k (runLspTc lcEnv)

instance MonadLsp Config LspTc where
    getLspEnv :: LspTc (LanguageContextEnv Config)
    getLspEnv = LspTc $ \lcEnv -> pure lcEnv

--------------------------------------------------------------------------------
-- Language-Server Configuration
--------------------------------------------------------------------------------

type Config :: Type
data Config = Config
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
        pure Config{..}

instance ToJSON Config where
    toJSON :: Config -> Value
    toJSON Config{..} =
        object
            [ "maxNumberOfProblems" .= maxNumberOfProblems
            ]

parseConfig :: Config -> Value -> Either Text Config
parseConfig _oldConfig newConfigRaw =
    case fromJSON newConfigRaw of
        Error errorMessage -> Left (T.pack errorMessage)
        Success newConfig -> Right newConfig

onConfigChange :: Config -> LspTc ()
onConfigChange _newConfig = pure ()
