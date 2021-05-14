{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Core.Compile.DataFlow where

import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT, local)
import Control.Monad.State (MonadState(..), StateT, runStateT, evalStateT, modify)
import Control.Monad.Writer (WriterT, runWriterT)
import Control.Monad.Trans (MonadTrans(..))
import Vehicle.Core.AST

-- |Encapsulates the data-flow for most passes along the syntax tree.
--
--
type family DATAFLOW (s :: *) (m :: * -> *) (sorted :: Sort -> *) (sort :: Sort) where
  DATAFLOW s m sorted 'KIND =           m (sorted 'KIND) -- no information
  DATAFLOW s m sorted 'TYPE = ReaderT s m (sorted 'TYPE) -- read-only
  DATAFLOW s m sorted 'TARG = WriterT s m (sorted 'TARG) -- write-only
  DATAFLOW s m sorted 'EXPR = ReaderT s m (sorted 'EXPR) -- read-only
  DATAFLOW s m sorted 'EARG = WriterT s m (sorted 'EARG) -- write-only
  DATAFLOW s m sorted 'DECL = StateT  s m (sorted 'DECL) -- read-write
  DATAFLOW s m sorted 'PROG = ReaderT s m (sorted 'PROG) -- read-only

newtype DataFlow (s :: *) (m :: * -> *) (sorted :: Sort -> *) (sort :: Sort)
  = DF { unDF :: DATAFLOW s m sorted sort }

type family RUNDF (s :: *) (m :: * -> *) (sorted :: Sort -> *) (sort :: Sort) where
  RUNDF s m sorted 'KIND =      m (sorted 'KIND   )
  RUNDF s m sorted 'TYPE = s -> m (sorted 'TYPE   )
  RUNDF s m sorted 'TARG =      m (sorted 'TARG, s)
  RUNDF s m sorted 'EXPR = s -> m (sorted 'EXPR   )
  RUNDF s m sorted 'EARG =      m (sorted 'EARG, s)
  RUNDF s m sorted 'DECL = s -> m (sorted 'DECL, s)
  RUNDF s m sorted 'PROG = s -> m (sorted 'PROG   )

runDF :: KnownSort sort => DataFlow s m sorted sort -> RUNDF s m sorted sort
runDF (m :: DataFlow s m sorted sort) = case sortSing @sort of
  SKIND ->              unDF m
  STYPE -> runReaderT $ unDF m
  STARG -> runWriterT $ unDF m
  SEXPR -> runReaderT $ unDF m
  SEARG -> runWriterT $ unDF m
  SDECL -> runStateT  $ unDF m
  SPROG -> runReaderT $ unDF m

-- |RunDF a |DataFlow| object, ignoring any output.
toReader :: (Monad m, KnownSort sort) => DataFlow s m sorted sort -> s -> m (sorted sort)
toReader (m :: DataFlow s m sorted sort) s = case sortSing @sort of
  SKIND ->         runDF m
  STYPE ->         runDF m s
  STARG -> fst <$> runDF m
  SEXPR ->         runDF m s
  SEARG -> fst <$> runDF m
  SDECL -> fst <$> runDF m s
  SPROG ->         runDF m s

-- |Convert a reader monad to a state monad.
readerToState :: Monad m => ReaderT s m a -> StateT s m a
readerToState m = do x <- get; lift (runReaderT m x)

-- |Convert a writer monad to a state monad.
writerToState :: (Monoid s, Monad m) => WriterT s m a -> StateT s m a
writerToState m = do (x, s) <- lift $ runWriterT m; modify (s<>); return x

-- |Convert a writer monad to a local change in a reader monad.
writerToReader :: (Monoid s, Monad m) => WriterT s m a -> (a -> ReaderT s m b) -> ReaderT s m b
writerToReader m k = do (x, s) <- lift $ runWriterT m; local (s<>) (k x)

-- |Convert a state monad to a reader monad.
stateToReader :: Monad m => StateT s m a -> ReaderT s m a
stateToReader m = do x <- ask; lift (evalStateT m x)
