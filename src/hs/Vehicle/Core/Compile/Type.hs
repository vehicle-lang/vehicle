{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Vehicle.Core.Compile.Type where

import           Control.Monad.Except (Except, MonadError(..))
import           Control.Monad.Reader (ReaderT, MonadReader(..))
import           Control.Monad.State (StateT , MonadState(..))
import           Control.Monad.Trans (MonadTrans(..))
import           Control.Monad.Writer (WriterT, runWriterT, MonadWriter(..))
import           Data.Sequence (Seq, (!?))
import           Vehicle.Core.AST
import           Vehicle.Core.Compile.DataFlow
import           Vehicle.Core.Compile.Provenance (Provenance)
import           Vehicle.Prelude


-- * Errors thrown during type checking

data TypeError
  = IndexOutOfBounds Provenance Index
  | forall sort. KnownSort sort =>
    Mismatch { actual :: INFO sort, expected :: INFO sort }

indexOutOfBounds ::
  (MonadError TypeError m, KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K Provenance sort -> DeBruijn sort -> m a
indexOutOfBounds (K p) db =
  throwError $ IndexOutOfBounds p (toIndex db)

mismatch :: forall sort m a. (MonadError TypeError m, KnownSort sort) => INFO sort -> INFO sort -> m a
mismatch actual expected = throwError $ Mismatch @sort actual expected


-- * Type information

-- |Type information, based on sort.
newtype Info (sort :: Sort) = I { unI :: INFO sort }

-- |Computes type information based on sort; kinds for types, types for expressions.
type family INFO (sort :: Sort) where
  INFO 'KIND = ()
  INFO 'TYPE = AKind (Info :*: K Provenance)
  INFO 'TARG = AKind (Info :*: K Provenance)
  INFO 'EXPR = AType (Info :*: K Provenance)
  INFO 'EARG = AType (Info :*: K Provenance)
  INFO 'DECL = ()
  INFO 'PROG = ()

instance KnownSort sort => Eq (Info sort) where
  I info1 == I info2 = case sortSing @sort of
    SKIND -> info1 == info2
    STYPE -> info1 == info2
    STARG -> info1 == info2
    SEXPR -> info1 == info2
    SEARG -> info1 == info2
    SDECL -> info1 == info2
    SPROG -> info1 == info2


-- * Type contexts

-- |Type context.
data Ctx = Ctx { typeInfo :: Seq (Info 'TYPE), exprInfo :: Seq (Info 'EXPR) }

instance Semigroup Ctx where
  Ctx typeInfo1 exprInfo1 <> Ctx typeInfo2 exprInfo2 =
    Ctx (typeInfo1 <> typeInfo2) (exprInfo1 <> exprInfo2)

instance Monoid Ctx where
  mempty = Ctx mempty mempty

-- |Get the sub-context for a given sort.
getSubCtxFor :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => Ctx -> Seq (Info sort)
getSubCtxFor Ctx{..} = case sortSing @sort of { STYPE -> typeInfo; SEXPR -> exprInfo }

-- |Find the type information for a given deBruijn index of a given sort.
getInfo ::
  forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K Provenance sort ->
  DeBruijn sort ->
  ReaderT Ctx (Except TypeError) (Info sort)
getInfo p db = do
  let idx = toIndex db
  subctx <- getSubCtxFor @sort <$> ask
  maybe (indexOutOfBounds p db) return (subctx !? idx)

noInfo :: forall sort. (KnownSort sort, sort `In` ['KIND, 'DECL, 'PROG]) => Info sort
noInfo = case sortSing @sort of { SKIND -> I (); SDECL -> I (); SPROG -> I () }

-- *

-- |Type of a checking algorithm.
--
-- A computation of the |Check| type returns a tree annotated with typing information or throws an exception.
-- Furthermore, there are two different types of data threaded through the computation. The |DATAFLOW Ctx|
-- construct threads a typing context through the computation, as appropriate for the sort we're checking,
-- e.g., expression have read and /local/ write access to the context. The |ReaderT (INFO sort)| construct
-- provides the information to check against.
--
newtype Check (sort :: Sort)
  = Chk { unChk ::
            (DataFlow Ctx)
            (ReaderT (INFO sort) (Except TypeError))
            (Tree DeBruijn Builtin (Info :*: K Provenance))
            sort
        }

-- |Type of an inference algorithm.
--
-- A computation of the |Infer| type is similar to the |Check| type, but instead of /requiring/ the
-- information to check against via a reader monad, it /provides/ said information via a writer monad.
newtype Infer (sort :: Sort)
  = Inf { unInf ::
            (DataFlow Ctx)
            (WriterT (INFO sort) (Except TypeError))
            (Tree DeBruijn Builtin (Info :*: K Provenance))
            sort
        }

inferToCheck ::
  forall sort a.
  (Eq (INFO sort), KnownSort sort) =>
  WriterT (INFO sort) (Except TypeError) a ->
  ReaderT (INFO sort) (Except TypeError) a
inferToCheck inf = do
  (x, act) <- lift $ runWriterT inf
  exp <- ask
  if act == exp then return x else mismatch @sort act exp

infer ::
  forall sort.
  (Eq (INFO sort), Monoid (INFO sort), KnownSort sort) =>
  Infer sort ->
  (Check :*: Infer) sort
infer inf = chk :*: inf
  where
    chk :: Check sort
    chk = Chk $ mapDF (inferToCheck @sort) (unInf inf)

checkInferF ::
  forall sort.
  (KnownSort sort) =>
  ATreeF (K Provenance) sort (Check :*: Infer) ->
  (Check :*: Infer) sort

checkInferF = case sortSing @sort of

  SKIND -> \case
    KAppF  ann k1 k2 -> undefined
    KConF  ann op    -> undefined
    KMetaF ann i     -> undefined

  STYPE -> \case
    TForallF  ann n t   -> undefined
    TAppF     ann t1 t2 -> undefined
    TVarF     ann n     -> undefined
    TConF     ann op    -> undefined
    TLitDimF  ann d     -> undefined
    TLitListF ann ts    -> undefined
    TMetaF    ann i     -> undefined

  STARG -> undefined

  SEXPR -> undefined

  SEARG -> undefined

  SDECL -> undefined

  SPROG -> undefined


-- * Helper functions

-- return2 :: (Monad m1, Monad m2) => a -> (m1 a, m2 a)
-- return2 x = (return x, return x)

-- (<$$>) :: (Functor f1, Functor f2) => (a -> b) -> (f1 a, f2 a) -> (f1 b, f2 b)
-- (<$$>) f (x, y) = (f <$> x, f <$> y)

-- (<**>) :: (Applicative f1, Applicative f2) => (f1 (a -> c), f2 (b -> d)) -> (f1 a, f2 b) -> (f1 c, f2 d)
-- (<**>) (f, g) (a, b) = (f <*> a, g <*> b)

-- -}
-- -}
-- -}
-- -}
-- -}
