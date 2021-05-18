{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE LiberalTypeSynonyms       #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Vehicle.Core.Compile.Type where

import           Control.Monad.Except (Except, MonadError(..))
import           Control.Monad.Reader (ReaderT, runReaderT, MonadReader(..), asks)
import           Control.Monad.Trans (MonadTrans(..))
import           Control.Monad.Writer (WriterT, runWriterT, MonadWriter(..))
import           Data.Sequence (Seq, (!?))
import           Vehicle.Core.AST
import           Vehicle.Core.Compile.DataFlow
import           Vehicle.Prelude

-- TODO:
--
--  - Extend contexts with information for meta-variables.
--  - Add support for unification with meta-variables.
--

-- * Errors thrown during type checking

data TypeError
  = IndexOutOfBounds
      Provenance  -- ^ The location of the deBruijn index.
      Index       -- ^ The deBruijn index.
  | forall sort. KnownSort sort =>
    Mismatch
      Provenance  -- ^ The location of the mismatch.
      (INFO sort) -- ^ The inferred type.
      (INFO sort) -- ^ The expected type.
  | MissingAnnotation
      Provenance  -- ^ The location of the missing annotation.
  | UnsupportedOperation
      Provenance  -- ^ The location of the unsupported operation.

-- |Throws an |UnsupportedOperation| exception.
unsupportedOperation :: KnownSort sort => K Provenance sort -> (Check :*: Infer) sort
unsupportedOperation p = Chk uoDF :*: Inf uoDF
  where
    uoDF :: (KnownSort sort, Monoid s, MonadError TypeError m) => DataFlow s m sorted sort
    uoDF = liftDF . throwError . UnsupportedOperation . unK $ p


-- * Type contexts

-- |Type context.
data Ctx = Ctx { typeInfo :: Seq (Info 'TYPE), exprInfo :: Seq (Info 'EXPR) }

instance Semigroup Ctx where
  Ctx ti1 ei1 <> Ctx ti2 ei2 = Ctx (ti1 <> ti2) (ei1 <> ei2)

instance Monoid Ctx where
  mempty = Ctx mempty mempty

-- |Create a context with a single piece of information of the given sort.
singletonCtx :: (sort `In` ['TYPE, 'EXPR]) => SSort sort -> Info sort -> Ctx
singletonCtx STYPE info = Ctx (Seq.singleton info) Seq.empty
singletonCtx SEXPR info = Ctx Seq.empty (Seq.singleton info)

-- |Get the sub-context for a given sort.
getSubCtxFor :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => Ctx -> Seq (Info sort)
getSubCtxFor = case sortSing @sort of STYPE -> typeInfo; SEXPR -> exprInfo

-- |Find the type information for a given deBruijn index of a given sort.
getInfo ::
  forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K Provenance sort -> DeBruijn sort -> INFER Info sort
getInfo p db = do
  subctx <- getSubCtxFor @sort <$> getData
  let index = toIndex db
  let maybeInfo = subctx !? index
  maybe (throwError $ IndexOutOfBounds (unK p) index) return maybeInfo


{-
-- * Bidirectional type checking and inference algorithm

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

type CHECK (sorted :: Sort -> *) (sort :: Sort)
  = (DATAFLOW Ctx)
    (ReaderT (INFO sort) (Except TypeError))
    (Tree DeBruijn Builtin (Info :*: K Provenance))
    sort

-- |Type of an inference algorithm.
--
-- A computation of the |Infer| type is similar to the |Check| type, but instead of /requiring/ the
-- information to check against via a reader monad, it /provides/ said information via a writer monad.
--
newtype Infer (sort :: Sort)
  = Inf { unInf ::
            (DataFlow Ctx)
            (Except TypeError)
            (Tree DeBruijn Builtin (Info :*: K Provenance) :*: Info)
            sort
        }

type INFER (sorted :: Sort -> *) (sort :: Sort)
  = (DATAFLOW Ctx)
    (Except TypeError)
    (Tree DeBruijn Builtin (Info :*: K Provenance) :*: Info)
    sort



-- Roughly:
--
-- - Introduction forms are checked
-- - Elimination forms and variables are inferred
-- - if_then_else can be checkable
-- - variable with polymorphic type
-- - union-find algorithm for unification and substitution

checkInferF ::
  forall sort.
  (KnownSort sort) =>
  ATreeF (K Provenance) sort (Check :*: Infer) ->
  (Check :*: Infer) sort

checkInferF = case sortSing @sort of

  -- Kinds
  --
  -- NOTE: Kinds are always well-formed, and they don't store any useful typing information.
  --       However, we /must/ iterate over kinds to add the singleton information |noInfo|.
  --       We implement this using |withInfer| so that we automatically get a valid checking
  --       algorithm out of it as well -- the other way around would result in an error when
  --       invoking the inference algorithm.
  --
  SKIND -> \case
    KAppF  p k1 k2 -> _
    KConF  p op    -> _
    KMetaF p _i    -> unsupportedOperation p

  -- Types
  STYPE -> \case
    TForallF     p n t   -> withInfer p $ undefined
    TAppF        p t1 t2 -> withInfer p $ undefined
    TVarF        p n     -> withInfer p $ undefined
      -- info <- getInfo p n
      -- tell [unInfo info]
      -- return $ TVar (info :*: p) n

    TConF        p op    -> withInfer p $ undefined -- return $ TCon (kindOf op :*: p) op
    TLitDimF     p d     -> withInfer p $ undefined -- return $ TLitDim (kDim :*: p) d
    TLitDimListF p ts    -> withInfer p $ undefined -- TLitDimList (kDimList :*: p) <$> traverse (runCheckAsInfer kDim) ts
    TMetaF       p i     -> unsupportedOperation p

  -- Type argument
  STARG -> \case
    TArgF p n -> undefined

  -- Expressions
  SEXPR -> \case
    EAnnF     p e t     -> undefined
    ELetF     p n e1 e2 -> undefined
    ELamF     p n e     -> undefined
    EAppF     p e1 e2   -> undefined
    EVarF     p n       -> undefined
    ETyAppF   p e t     -> undefined
    ETyLamF   p n e     -> undefined
    EConF     p op      -> undefined
    ELitIntF  p z       -> undefined
    ELitRealF p r       -> undefined
    ELitSeqF  p es      -> undefined

  -- Expression arguments
  SEARG -> \case
    EArgF p n -> undefined

  -- Declarations
  SDECL -> \case
    DeclNetwF p n t    -> undefined
    DeclDataF p n t    -> undefined
    DefTypeF  p n ns t -> undefined
    DefFunF   p n t e  -> undefined

  -- Programs
  SPROG -> \case
    MainF p ds -> undefined



-- |Return the kind for builtin types.
kindOf :: Builtin 'TYPE -> Info 'TYPE
kindOf = \case
  TFun    -> kType ~> kType ~> kType
  TBool   -> kType
  TProp   -> kType
  TInt    -> kType
  TReal   -> kType
  TList   -> kType ~> kType
  TTensor -> kDim ~> kType ~> kType
  TAdd    -> kDim ~> kDim ~> kDim
  TCons   -> kDim ~> kDimList ~> kDimList


-- * A tiny DSL for writing kinds and types as |Info| expressions

infixl 3 `kApp`

kApp :: Info 'TYPE -> Info 'TYPE -> Info 'TYPE
kApp = liftInfo2 $ KApp mempty

infixr 4 ~>

(~>) :: Info 'TYPE -> Info 'TYPE -> Info 'TYPE
k1 ~> k2 = kFun `kApp` k1 `kApp` k2

kFun, kType, kDim, kDimList :: Info 'TYPE
kFun     = Info $ KCon mempty KFun
kType    = Info $ KCon mempty KType
kDim     = Info $ KCon mempty KDim
kDimList = Info $ KCon mempty KDimList


-- * Combinators for writing bidirectional typing algorithms

-- |Runs the checking portion of a check-and-infer algorithm.
runCheck ::
  (Check :*: Infer) sort ->
  CHECK (Tree DeBruijn Builtin (Info :*: K Provenance)) sort
runCheck chkInf = unDF (unChk (ifst chkInf))

-- |Runs the inference portion of a check-and-infer algorithm.
runInfer ::
  (Check :*: Infer) sort ->
  INFER (Tree DeBruijn Builtin (Info :*: K Provenance) :*: Info) sort
runInfer = undefined -- unDF . unInf . isnd

-- |Runs the checking portion of a check-and-infer algorithm, then switches to inference.
runCheckAsInfer ::
  forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR, 'PROG]) =>
  Info sort -> (Check :*: Infer) sort -> INFER (Tree DeBruijn Builtin (Info :*: K Provenance)) sort
runCheckAsInfer info chkInf = undefined

  -- unDF . unInf $ inf
  -- where
  --   chk :: Check sort
  --   chk = ifst chkInf
  --   inf :: Infer sort
  --   inf = Inf . mapDF (lift @(WriterT _) . flip runReaderT (unInfo info)) . unChk $ chk

-- |Creates a combined check-and-infer pass from a checking pass.
withCheck ::
  forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR, 'PROG]) =>
  K Provenance sort -> CHECK (Tree DeBruijn Builtin (Info :*: K Provenance)) sort -> (Check :*: Infer) sort
withCheck p chk = undefined

  -- Chk (DF chk) :*: inf
  -- where
  --   inf :: Infer sort
  --   inf = Inf . liftDF . throwError $ MissingAnnotation (unK p)

-- |Creates a combined check-and-infer pass from an inference pass, via |inferToCheck|.
withInfer ::
  forall sort. (Eq (INFO sort), KnownSort sort) =>
  K Provenance sort -> INFER (Tree DeBruijn Builtin (Info :*: K Provenance)) sort -> (Check :*: Infer) sort
withInfer p inf = undefined

  -- chk :*: Inf (DF inf)
  -- where
  --   chk :: Check sort
  --   chk = Chk $ mapDF (inferToCheck p) (DF inf)

  --   -- Converts an inference pass to a checking pass by comparing the inferred
  --   -- information against the checked information.
  --   inferToCheck ::
  --     forall sort a. (Eq (INFO sort), KnownSort sort) =>
  --     K Provenance sort ->
  --     Except TypeError (a, INFO sort) ->
  --     ReaderT (INFO sort) (Except TypeError) a
  --   inferToCheck p inf = do
  --     (x, inferred) <- lift inf
  --     expected <- ask
  --     if inferred == expected
  --       then return x
  --       else throwError $ Mismatch @sort (unK p) inferred expected

-- -}
-- -}
-- -}
-- -}
-- -}
