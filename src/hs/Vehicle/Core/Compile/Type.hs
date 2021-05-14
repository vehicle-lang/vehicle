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
  = TI { unTI :: INFO sort }

-- |Computes type information based on sort; kinds for types, types for expressions.
type family INFO (sort :: Sort) where
  INFO 'KIND = ()
  INFO 'TYPE = AKind (Info :*: K Provenance)
  INFO 'TARG = AKind (Info :*: K Provenance)
  INFO 'EXPR = AType (Info :*: K Provenance)
  INFO 'EARG = AType (Info :*: K Provenance)
  INFO 'DECL = ()
  INFO 'PROG = ()


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

{-
data TypeError

-- |Type context.
data Ctx = Ctx { tEnv :: [INFO 'TYPE] , eEnv :: [INFO 'EXPR] }

noInfo :: forall sort. (KnownSort sort, sort `In` ['KIND, 'DECL, 'PROG]) => Info sort
noInfo = case sortSing @sort of
  SKIND -> TI ()
  SDECL -> TI ()
  SPROG -> TI ()

-- |Type information annotation.
newtype Info (sort :: Sort) = TI { unTI :: INFO sort }

type family INFO (sort :: Sort) where
  INFO 'KIND = ()
  INFO 'TYPE = AKind (K Provenance)
  INFO 'TARG = AKind (K Provenance)
  INFO 'EXPR = AType (K Provenance)
  INFO 'EARG = AType (K Provenance)
  INFO 'DECL = ()
  INFO 'PROG = ()

-- |Type of well-formed abstract trees with type and provenance information.
type WfTree (sort :: Sort) = ATree (Info :*: K Provenance) sort

type WfKind = WfTree 'KIND
type WfType = WfTree 'TYPE
type WfTArg = WfTree 'TARG
type WfExpr = WfTree 'EXPR
type WfEArg = WfTree 'EARG
type WfDecl = WfTree 'DECL
type WfProg = WfTree 'PROG

-- |Type functor for well-formed abstract trees.
type WfTreeF (sort :: Sort) (sorted :: Sort -> *) = ATreeF (Info :*: K Provenance) sort sorted

type WfKindF (sorted :: Sort -> *) = WfTreeF 'KIND sorted
type WfTypeF (sorted :: Sort -> *) = WfTreeF 'TYPE sorted
type WfTArgF (sorted :: Sort -> *) = WfTreeF 'TARG sorted
type WfExprF (sorted :: Sort -> *) = WfTreeF 'EXPR sorted
type WfEArgF (sorted :: Sort -> *) = WfTreeF 'EARG sorted
type WfDeclF (sorted :: Sort -> *) = WfTreeF 'DECL sorted
type WfProgF (sorted :: Sort -> *) = WfTreeF 'PROG sorted

-- |Type of checking operations, parameterised by a monad transformer.
type Check (sort :: Sort) = Reader (INFO sort)

-- |Type of inferring operations, parameterised by a monad transformer.
type Infer (sort :: Sort) = Writer (INFO sort)

type family DataFlow (info :: *) (sort :: Sort) :: (* -> *) -> (* -> *) where
  DataFlow info 'KIND = IdentityT    -- no information
  DataFlow info 'TYPE = ReaderT info -- read-only
  DataFlow info 'TARG = WriterT info -- write-only
  DataFlow info 'EXPR = ReaderT info -- read-only
  DataFlow info 'EARG = WriterT info -- write-only
  DataFlow info 'DECL = StateT  info -- read-write
  DataFlow info 'PROG = ReaderT info -- read-only

type CHECKINFER (sort :: Sort) =
  ( (DataFlow Ctx sort) (Check sort) (WfTree sort),
    (DataFlow Ctx sort) (Infer sort) (WfTree sort))

newtype CheckInfer (sort :: Sort) = CI { unCI :: CHECKINFER sort }

checkInferF ::
  forall sort.
  (KnownSort sort) =>
  ATreeF (K Provenance) sort CheckInfer ->
  CHECKINFER sort

checkInferF = case sortSing @sort of

  SKIND -> \case
    KAppF  ann k1 k2 -> let (k1c, k1i) = unCI k1
                            (k2c, k2i) = unCI k2
                            ann' = noInfo :*: ann
                        in  ( KApp ann' <$> k1c <*> k2c
                            , KApp ann' <$> k1i <*> k2i
                            )
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
