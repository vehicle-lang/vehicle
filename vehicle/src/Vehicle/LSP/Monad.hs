module Vehicle.LSP.Monad
  ( LspTc (..),
    onConfigChange,
    runLspTc,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import GHC.Exts (oneShot)
import Language.LSP.Server (LanguageContextEnv, MonadLsp (..))
import Vehicle.LSP.Config

--------------------------------------------------------------------------------
-- Language-Server Type-Checker Monad Stack
--
-- The `LspTc` monad uses a one-shot reader encoding. For details, see the GHC
-- Note [The one-shot state monad trick] or https://github.com/ghc/ghc/blob/
-- ab3ab3e3d489a351e84f4fe681de1731549376a2/compiler/GHC/Utils/Monad.hs#L259
--------------------------------------------------------------------------------

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
  fmap f (LspTc ma) = LspTc (fmap f . ma)

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
  liftIO m = LspTc $ const m

instance MonadUnliftIO LspTc where
  withRunInIO :: ((forall a. LspTc a -> IO a) -> IO b) -> LspTc b
  withRunInIO k = LspTc $ \lcEnv -> k (runLspTc lcEnv)

instance MonadLsp Config LspTc where
  getLspEnv :: LspTc (LanguageContextEnv Config)
  getLspEnv = LspTc $ \lcEnv -> pure lcEnv

onConfigChange :: Config -> LspTc ()
onConfigChange _newConfig = pure ()
