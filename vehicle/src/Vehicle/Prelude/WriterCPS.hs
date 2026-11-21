{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | mtl only provides class instances for the CPS writer transformer from
-- `Control.Monad.Trans.Writer.CPS` from version 2.3 onwards. GHC 9.4 ships
-- with mtl-2.2.2, so we provide the instances ourselves for older mtls.
module Vehicle.Prelude.WriterCPS () where

#if !MIN_VERSION_mtl(2,3,0)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Writer (MonadWriter (..))

instance (Monoid w, Monad m) => MonadWriter w (CPS.WriterT w m) where
  writer = CPS.writer
  tell = CPS.tell
  listen = CPS.listen
  pass = CPS.pass

instance (Monoid w, MonadReader r m) => MonadReader r (CPS.WriterT w m) where
  ask = lift ask
  local = CPS.mapWriterT . local
  reader = lift . reader

instance (Monoid w, MonadState s m) => MonadState s (CPS.WriterT w m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (Monoid w, MonadError e m) => MonadError e (CPS.WriterT w m) where
  throwError = lift . throwError
  catchError = CPS.liftCatch catchError
#endif
