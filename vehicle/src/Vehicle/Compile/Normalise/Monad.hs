{-# HLINT ignore "Use <|>" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.Monad where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (ReaderT (..), asks)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans (..))
import Control.Monad.Writer (WriterT)
import Data.Data (Proxy (..))
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print
import Vehicle.Compile.Type.Core
import Vehicle.Expr.BuiltinInterface

-----------------------------------------------------------------------------
-- Normalisation monad

newtype EvalOptions = EvalOptions
  { evalFiniteQuantifiers :: Bool
  }

defaultEvalOptions :: EvalOptions
defaultEvalOptions =
  EvalOptions
    { evalFiniteQuantifiers = True
    }

class (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) => MonadNorm builtin m where
  getEvalOptions :: Proxy builtin -> m EvalOptions
  getDeclSubstitution :: m (NormDeclCtx builtin)

instance (MonadNorm builtin m) => MonadNorm builtin (StateT s m) where
  getEvalOptions = lift . getEvalOptions
  getDeclSubstitution = lift getDeclSubstitution

instance (Monoid s, MonadNorm builtin m) => MonadNorm builtin (WriterT s m) where
  getEvalOptions = lift . getEvalOptions
  getDeclSubstitution = lift getDeclSubstitution

instance (MonadNorm builtin m) => MonadNorm builtin (ReaderT s m) where
  getEvalOptions = lift . getEvalOptions
  getDeclSubstitution = lift getDeclSubstitution

newtype NormT builtin m a = NormT
  { unnormT :: ReaderT (EvalOptions, NormDeclCtx builtin) m a
  }
  deriving (Functor, Applicative, Monad)

runNormT :: EvalOptions -> NormDeclCtx builtin -> NormT builtin m a -> m a
runNormT opts declSubst x = runReaderT (unnormT x) (opts, declSubst)

runEmptyNormT :: NormT builtin m a -> m a
runEmptyNormT = runNormT defaultEvalOptions mempty

instance MonadTrans (NormT builtin) where
  lift = NormT . lift

instance (MonadLogger m) => MonadLogger (NormT builtin m) where
  setCallDepth = lift . setCallDepth
  getCallDepth = lift getCallDepth
  incrCallDepth = lift incrCallDepth
  decrCallDepth = lift decrCallDepth
  getDebugLevel = lift getDebugLevel
  logMessage = lift . logMessage

instance (MonadError e m) => MonadError e (NormT builtin m) where
  throwError = lift . throwError
  catchError m f = NormT (catchError (unnormT m) (unnormT . f))

instance (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) => MonadNorm builtin (NormT builtin m) where
  getEvalOptions _ = NormT $ asks fst
  getDeclSubstitution = NormT $ asks snd
