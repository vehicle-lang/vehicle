{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Vehicle.Core.Compile.Dataflow where

import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Except (MonadError(..))
import Control.Monad.Identity (IdentityT(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.State (MonadState(..), StateT(..), modify, evalStateT)
import Control.Monad.Supply (MonadSupply(..))
import Control.Monad.Writer (MonadWriter(..), WriterT(..))
import Vehicle.Core.AST
import Vehicle.Prelude

-- |Encapsulates the dataflow for a checking algorithm on sorted trees.
newtype SortedDataflowT (s :: *) (m :: * -> *) (sorted :: Sort -> *) (sort :: Sort)
  = SDF { unSDF :: DataflowT sort s m (sorted sort) }
  -- unSDF -> unwrapSortedDataflowT

-- |Encapsulates the dataflow for a checking algorithm on sorted trees.
newtype DataflowT (sort :: Sort) (s :: *) (m :: * -> *) (a :: *)
  = DF { unDF :: DATAFLOW sort s m a }

type family DATAFLOW (sort :: Sort) (s :: *) :: (* -> *) -> (* -> *) where
  DATAFLOW 'KIND s = IdentityT
  DATAFLOW 'TYPE s = ReaderT s
  DATAFLOW 'TARG s = WriterT s
  DATAFLOW 'EXPR s = ReaderT s
  DATAFLOW 'EARG s = WriterT s
  DATAFLOW 'DECL s = StateT  s
  DATAFLOW 'PROG s = IdentityT

instance (KnownSort sort) => MonadTrans (DataflowT sort s) where
  lift = case sortSing @sort of
    SKIND -> lift
    STYPE -> lift
    STARG -> lift
    SEXPR -> lift
    SEARG -> lift
    SDECL -> lift
    SPROG -> lift

instance (KnownSort sort, Functor f) => Functor (DataflowT sort s f) where
  fmap = case sortSing @sort of
    SKIND -> fmap
    STYPE -> fmap
    STARG -> fmap
    SEXPR -> fmap
    SEARG -> fmap
    SDECL -> fmap
    SPROG -> fmap

instance (KnownSort sort, Applicative f) => Applicative (DataflowT sort s f) where
  pure = case sortSing @sort of
    SKIND -> pure
    STYPE -> pure
    STARG -> pure
    SEXPR -> pure
    SEARG -> pure
    SDECL -> pure
    SPROG -> pure

  (<*>) = case sortSing @sort of
    SKIND -> (<*>)
    STYPE -> (<*>)
    STARG -> (<*>)
    SEXPR -> (<*>)
    SEARG -> (<*>)
    SDECL -> (<*>)
    SPROG -> (<*>)

instance (KnownSort sort, Monad m) => Monad (DataflowT sort s m) where
  (>>=) = case sortSing @sort of
    SKIND -> (>>=)
    STYPE -> (>>=)
    STARG -> (>>=)
    SEXPR -> (>>=)
    SEARG -> (>>=)
    SDECL -> (>>=)
    SPROG -> (>>=)

instance (KnownSort sort, MonadError e m) => MonadError e (DataflowT sort s m) where
  throwError = lift . throwError
  catchError = case sortSing @sort of
    SKIND -> catchError
    STYPE -> catchError
    STARG -> catchError
    SEXPR -> catchError
    SEARG -> catchError
    SDECL -> catchError
    SPROG -> catchError

instance (KnownSort sort, MonadSupply s f m) => MonadSupply s f (DataflowT sort d m) where
  supply  = lift . supply
  provide = lift . provide

-- TODO: change these instances to redirect to the underlying monad

instance (KnownSort sort, sort `In` ['TYPE, 'EXPR], Monad m) => MonadReader r (DataflowT sort r m) where
  ask     = fromReaderT ask
  local k = fromReaderT . local k . toReaderT

instance (KnownSort sort, sort `In` ['TARG, 'EARG], Monoid s, Monad m) => MonadWriter s (DataflowT sort s m) where
  tell   = fromWriterT . tell
  listen = fromWriterT . listen . toWriterT
  pass   = fromWriterT . pass   . toWriterT

runDataflowT :: forall sort m s a. (KnownSort sort, Monad m) => s -> DataflowT sort s m a -> m (a, s)
runDataflowT input (DF x) = case sortSing @sort of
    SKIND -> do y <- runIdentityT x; return (y, input)
    STYPE -> do y <- runReaderT x input; return (y, input)
    STARG -> do (y, output) <- runWriterT x; return (y, output)
    SEXPR -> do y <- runReaderT x input; return (y, input)
    SEARG -> do (y, output) <- runWriterT x; return (y, output)
    SDECL -> do (y, output) <- runStateT x input; return (y, output)
    SPROG -> do y <- runIdentityT x; return (y, input)

evalDataflowT :: forall sort m s a. (KnownSort sort, Monad m) => s -> DataflowT sort s m a -> m a
evalDataflowT input df = fst <$> runDataflowT input df

askData ::
  forall sort s m.
  (KnownSort sort, sort `In` ['TYPE, 'EXPR], Monad m) =>
  DataflowT sort s m s
askData = fromReaderT ask

tellData ::
  forall sort s m.
  (KnownSort sort, sort `In` ['TARG, 'EARG], Monoid s, Monad m) =>
  s -> DataflowT sort s m ()
tellData = fromWriterT . tell


-- * Cast |DataflowT| to specific monad transformers

-- |Convert from the |IdentityT| transformer to the |DataflowT| transformer.
fromIdentityT ::
  forall sort s m a.
  (KnownSort sort, sort `In` ['KIND, 'PROG], Monad m) =>
  IdentityT m a -> DataflowT sort s m a
fromIdentityT = case sortSing @sort of SKIND -> DF; SPROG -> DF

-- |Convert from the |ReaderT| transformer to the |DataflowT| transformer.
fromReaderT ::
  forall sort s m a.
  (KnownSort sort, sort `In` ['TYPE, 'EXPR], Monad m) =>
  ReaderT s m a -> DataflowT sort s m a
fromReaderT = case sortSing @sort of STYPE -> DF; SEXPR -> DF

-- |Convert from the |WriterT| transformer to the |DataflowT| transformer.
fromWriterT ::
  forall sort s m a.
  (KnownSort sort, sort `In` ['TARG, 'EARG], Monad m) =>
  WriterT s m a -> DataflowT sort s m a
fromWriterT = case sortSing @sort of STARG -> DF; SEARG -> DF

-- |Convert from the |DataflowT| transformer to the |IdentityT| transformer.
toIdentityT ::
  forall sort s m a.
  (KnownSort sort, sort `In` ['KIND, 'PROG], Monad m) =>
  DataflowT sort s m a -> IdentityT m a
toIdentityT = case sortSing @sort of SKIND -> unDF; SPROG -> unDF

-- |Convert from the |DataflowT| transformer to the |ReaderT| transformer.
toReaderT ::
  forall sort s m a.
  (KnownSort sort, sort `In` ['TYPE, 'EXPR], Monad m) =>
  DataflowT sort s m a -> ReaderT s m a
toReaderT = case sortSing @sort of STYPE -> unDF; SEXPR -> unDF

-- |Convert from the |DataflowT| transformer to the |WriterT| transformer.
toWriterT ::
  forall sort s m a.
  (KnownSort sort, sort `In` ['TARG, 'EARG], Monad m) =>
  DataflowT sort s m a -> WriterT s m a
toWriterT = case sortSing @sort of STARG -> unDF; SEARG -> unDF


-- * Convert between various dataflow monads


class Flow (sort1 :: Sort) (sort2 :: Sort) where

  -- |Convert dataflow for |sort1| to dataflow for |sort2|.
  flow :: (Monoid s, Monad m)
       => DataflowT sort1 s m a
       -> DataflowT sort2 s m a

instance Flow  sort  sort where flow = id
instance Flow 'KIND 'TYPE where flow = DF . identityToReader . unDF
instance Flow 'TYPE 'EXPR where flow = DF . unDF
instance Flow 'TARG 'DECL where flow = DF . writerToState . unDF
instance Flow 'TYPE 'DECL where flow = DF . readerToState . unDF
instance Flow 'EARG 'DECL where flow = DF . writerToState . unDF
instance Flow 'EXPR 'DECL where flow = DF . readerToState . unDF
instance Flow 'DECL 'PROG where flow = DF . stateToIdentity . unDF

-- |Variant of |flow| whose argument is a |SortedDataflowT|.
sflow :: (Flow sort1 sort2, Monoid s, Monad m)
      => SortedDataflowT s m sorted sort1
      -> DataflowT sort2 s m (sorted sort1)
sflow = flow . unSDF

-- |Alias for |flow|.
bind :: (Flow sort1 sort2, Monoid s, Monad m)
     => DataflowT sort1 s m a
     -> DataflowT sort2 s m a
bind = flow

-- |Alias for |sflow|.
sbind :: (Flow sort1 sort2, Monoid s, Monad m)
      => SortedDataflowT s m sorted sort1
      -> DataflowT sort2 s m (sorted sort1)
sbind = sflow

-- |Convert an identity monad to a reader monad.
identityToReader :: Monad m => IdentityT m a -> ReaderT s m a
identityToReader m = lift (runIdentityT m)

-- |Convert a reader monad to a state monad.
readerToState :: Monad m => ReaderT s m a -> StateT s m a
readerToState m = do x <- get; lift (runReaderT m x)

-- |Convert a writer monad to a state monad.
writerToState :: (Monoid s, Monad m) => WriterT s m a -> StateT s m a
writerToState m = do (x, s) <- lift (runWriterT m); modify (s<>); return x

-- |Convert a state monad to a reader monad.
stateToIdentity :: (Monoid s, Monad m) => StateT s m a -> IdentityT m a
stateToIdentity m = lift (evalStateT m mempty)

-- |Convert a writer monad to a local change in a reader monad.
writerToReaderLocal :: (Monoid s, Monad m) => WriterT s m a -> (a -> ReaderT s m b) -> ReaderT s m b
writerToReaderLocal m k = do (x, s) <- lift (runWriterT m); local (s<>) (k x)

-- |Bind the output of a writer monad locally in a reader monad.
bindLocal :: ( KnownSort wsort, wsort `In` ['TARG, 'EARG]
             , KnownSort rsort, rsort `In` ['TYPE, 'EXPR]
             , Monoid s, Monad m)
          => DataflowT wsort s m a
          -> (a -> DataflowT rsort s m b)
          -> DataflowT rsort s m b
bindLocal df k = fromReaderT $ writerToReaderLocal (toWriterT df) (toReaderT . k)

-- |Bind a list of outputs of a writer monad locally in a reader monad.
bindAllLocal :: ( KnownSort wsort, wsort `In` ['TARG, 'EARG]
                , KnownSort rsort, rsort `In` ['TYPE, 'EXPR]
                , Monoid s, Monad m)
             => [DataflowT wsort s m a]
             -> ([a] -> DataflowT rsort s m b)
             -> DataflowT rsort s m b
bindAllLocal []     k = k []
bindAllLocal (w:ws) k = bindLocal w (\r -> bindAllLocal ws (\rs -> k (r:rs)))

-- |Variant of |sbindLocal| whose argument is a |SortedDataflowT|.
sbindLocal :: ( KnownSort wsort, wsort `In` ['TARG, 'EARG]
              , KnownSort rsort, rsort `In` ['TYPE, 'EXPR]
              , Monoid s, Monad m)
           => SortedDataflowT s m sorted wsort
           -> (sorted wsort -> DataflowT rsort s m b)
           -> DataflowT rsort s m b
sbindLocal = bindLocal . unSDF

-- |Variant of |sbindAllLocal| whose argument is a |SortedDataflowT|.
sbindAllLocal :: ( KnownSort wsort, wsort `In` ['TARG, 'EARG]
                 , KnownSort rsort, rsort `In` ['TYPE, 'EXPR]
                 , Monoid s, Monad m)
              => [SortedDataflowT s m sorted wsort]
              -> ([sorted wsort] -> DataflowT rsort s m b)
              -> DataflowT rsort s m b
sbindAllLocal = bindAllLocal . fmap unSDF

-- -}
-- -}
-- -}
-- -}
-- -}
