{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE LiberalTypeSynonyms  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Vehicle.Core.Compile.Type where

import           Control.Arrow
import           Control.Monad.Except (Except, MonadError(..))
import           Control.Monad.Reader (Reader, ReaderT, MonadReader(..))
import           Control.Monad.State (State, StateT , MonadState(..))
import           Control.Monad.Writer (Writer, WriterT, MonadWriter(..))
import           Data.Sequence (Seq, (!?))
import           Vehicle.Core.AST
import           Vehicle.Core.Compile.DataFlow
import           Vehicle.Core.Compile.Provenance (Provenance)
import           Vehicle.Prelude


-- * Errors thrown during type checking

data TypeError
  = IndexOutOfBounds Provenance Index

indexOutOfBounds ::
  (MonadError TypeError m, KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K Provenance sort -> DeBruijn sort -> m a
indexOutOfBounds (K p) db =
  throwError $ IndexOutOfBounds p (toIndex db)


-- * Type information

-- |Type information, based on sort.
newtype Info (sort :: Sort)
  = I { unI :: INFO sort }

-- |Computes type information based on sort; kinds for types, types for expressions.
type family INFO (sort :: Sort) where
  INFO 'KIND = ()
  INFO 'TYPE = AKind (Info :*: K Provenance)
  INFO 'TARG = AKind (Info :*: K Provenance)
  INFO 'EXPR = AType (Info :*: K Provenance)
  INFO 'EARG = AType (Info :*: K Provenance)
  INFO 'DECL = ()
  INFO 'PROG = ()


-- * Type contexts

-- |Type context.
data Ctx = Ctx { typeInfo :: Seq (INFO 'TYPE), exprInfo :: Seq (INFO 'EXPR) }

instance Semigroup Ctx where
  Ctx typeInfo1 exprInfo1 <> Ctx typeInfo2 exprInfo2 =
    Ctx (typeInfo1 <> typeInfo2) (exprInfo1 <> exprInfo2)

instance Monoid Ctx where
  mempty = Ctx mempty mempty

-- |Get the sub-context for a given sort.
getSubCtxFor :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => Ctx -> Seq (INFO sort)
getSubCtxFor Ctx{..} = case sortSing @sort of { STYPE -> typeInfo; SEXPR -> exprInfo }

-- |Find the type information for a given deBruijn index of a given sort.
getInfo ::
  forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K Provenance sort ->
  DeBruijn sort ->
  ReaderT Ctx (Except TypeError) (INFO sort)
getInfo p db = do
  let idx = toIndex db
  subctx <- getSubCtxFor @sort <$> ask
  maybe (indexOutOfBounds p db) return (subctx !? idx)

noInfo :: forall sort. (KnownSort sort, sort `In` ['KIND, 'DECL, 'PROG]) => Info sort
noInfo = case sortSing @sort of { SKIND -> I (); SDECL -> I (); SPROG -> I () }

-- *

type Check (sort :: Sort) = ReaderT (INFO sort) (Except TypeError)
type Infer (sort :: Sort) = WriterT (INFO sort) (Except TypeError)

type CHECKINFER (sort :: Sort)
  = ( (DATAFLOW Ctx) (Check sort) (Tree DeBruijn Builtin (Info :*: K Provenance)) sort
    , (DATAFLOW Ctx) (Infer sort) (Tree DeBruijn Builtin (Info :*: K Provenance)) sort
    )

newtype CheckInfer (sort :: Sort)
  = CI { unCI :: CHECKINFER sort }

checkInferF ::
  forall sort.
  (KnownSort sort) =>
  ATreeF (K Provenance) sort CheckInfer ->
  CHECKINFER sort

checkInferF = case sortSing @sort of

  SKIND -> \case
    KAppF  ann k1 k2 -> _
    KConF  ann op    -> return &&& return $ KCon  (noInfo :*: ann) op
    KMetaF ann i     -> return &&& return $ KMeta (noInfo :*: ann) i

  STYPE -> undefined

  STARG -> undefined

  SEXPR -> undefined

  SEARG -> undefined

  SDECL -> undefined

  SPROG -> undefined

-- -}
-- -}
-- -}
-- -}
-- -}
