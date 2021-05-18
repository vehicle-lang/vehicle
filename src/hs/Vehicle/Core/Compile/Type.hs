{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE ImportQualifiedPost       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Vehicle.Core.Compile.Type where

import Control.Monad.Except (MonadError(..), Except)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Writer (MonadWriter(..), WriterT(..))
import Data.Sequence (Seq, (!?))
import Data.Sequence qualified as Seq
import Vehicle.Core.AST
import Vehicle.Core.Compile.Dataflow
import Vehicle.Prelude

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
      (Info sort) -- ^ The inferred type.
      (Info sort) -- ^ The expected type.
  | MissingAnnotation
      Provenance  -- ^ The location of the missing annotation.
  | UnsupportedOperation
      Provenance  -- ^ The location of the unsupported operation.


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
  K Provenance sort -> DeBruijn sort -> Infer sort ()
getInfo p db = Inf $ do
  subctx <- getSubCtxFor @sort <$> ask
  let ix = toIndex db
  let maybeInfo = subctx !? ix
  info <- maybe (throwError $ IndexOutOfBounds (unK p) ix) return maybeInfo
  return ((), info)


-- * Bidirectional type checking and inference algorithm

-- |The type of trees output by the type checking algorithm.
type CheckedTree (sort :: Sort) = Tree DeBruijn Builtin (Info :*: K Provenance) sort

-- |A checking pass, which combines a dataflow monad, a reader monad---providing
--  the information to check against---and an exception monad.
newtype Check (sort :: Sort) (a :: *)
  = Chk { unChk :: ReaderT (Info sort) (DataflowT sort Ctx (Except TypeError)) a }

instance KnownSort sort => Functor (Check sort) where
  fmap f (Chk x) = Chk (fmap f x)

instance KnownSort sort => Applicative (Check sort) where
  pure x = Chk (pure x)
  Chk f <*> Chk x = Chk (f <*> x)

instance KnownSort sort => Monad (Check sort) where
  Chk x >>= f = Chk (x >>= unChk . f)

instance KnownSort sort => MonadError TypeError (Check sort) where
  throwError e = Chk (throwError e)
  catchError (Chk x) k = Chk (catchError x (unChk . k))

-- |An inference pass, which combines a dataflow monad with an exception monad,
--  and returns a pair of the return value and the inferred information.
newtype Infer (sort :: Sort) (a :: *)
  = Inf { unInf :: DataflowT sort Ctx (Except TypeError) (a, Info sort) }

newtype SortedCheckInfer (sort :: Sort)
  = SCI { unSCI :: (Check sort (CheckedTree sort), Infer sort (CheckedTree sort)) }

-- |Check if a tree is well-kinded and well-typed, and insert typing information.
checkInfer ::
  forall sort. (KnownSort sort) =>
  Tree DeBruijn Builtin (K Provenance) sort ->
 (Check sort (CheckedTree sort), Infer sort (CheckedTree sort))
checkInfer = unSCI . foldTree (SCI . checkInferF)

-- |Check if a single layer is well-kinded and well-typed, see |checkInfer|.
checkInferF ::
  forall sort. (KnownSort sort) =>
  TreeF DeBruijn Builtin (K Provenance) sort SortedCheckInfer ->
  (Check sort (CheckedTree sort), Infer sort (CheckedTree sort))
checkInferF = case sortSing @sort of

  SKIND -> \case
    KAppF  p k1 k2 -> fromCheck p $ KApp (mempty :*: p) <$> runCheck k1 <*> runCheck k2
    KConF  p op    -> fromCheck p $ KCon (mempty :*: p) <$> pure op
    KMetaF p _i    -> fromCheck p $ throwError (UnsupportedOperation (unK p))

  -- Types
  STYPE -> \case
    TForallF     p n t   -> undefined
    TAppF        p t1 t2 -> undefined
    TVarF        p n     -> undefined
    TConF        p op    -> undefined
    TLitDimF     p d     -> undefined
    TLitDimListF p ts    -> undefined
    TMetaF       p i     -> undefined

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

-- |Constructed a bidirectional step from a checking step.
--
--  Concretely, the resulting algorithm always throws an error if the inference
--  portion is invoked, with the exception of sorts which have trivial
--  information, i.e., kinds, declarations, and programs, for which checking
--  and inference coincide.
--
fromCheck ::
  forall sort. (KnownSort sort) =>
  K Provenance sort -> Check sort (CheckedTree sort) ->
  (Check sort (CheckedTree sort), Infer sort (CheckedTree sort))
fromCheck p chk = (chk, checkToInfer p chk)
  where
    checkToInfer ::
      forall sort. (KnownSort sort) =>
      K Provenance sort -> Check sort (CheckedTree sort) -> Infer sort (CheckedTree sort)
    checkToInfer p chk = case sortSing @sort of
      SKIND -> inferNoop chk
      STYPE -> inferError p
      STARG -> inferError p -- TODO: could be implemented as something sensible?
      SEXPR -> inferError p
      SEARG -> inferError p -- TODO: could be implemented as something sensible?
      SDECL -> inferNoop chk
      SPROG -> inferNoop chk

    -- |An inference mode which always throws an error.
    inferError :: forall sort a. (KnownSort sort) => K Provenance sort -> Infer sort a
    inferError p = Inf $ throwError (MissingAnnotation (unK p))

    -- |For sorts with trivial information, checking and inference coincide into a no-op.
    inferNoop :: forall sort a. (KnownSort sort, Monoid (Info sort)) => Check sort a -> Infer sort a
    inferNoop chk = Inf $ do x <- runReaderT (unChk chk) mempty; return (x, mempty)

-- |Constructed a bidirectional step from an inference step.
--
--  Concretely, the resulting algorithm runs the inference mode, then compares
--  the inferred type to the expected type. If they're equal, it returns as
--  usual. If they're not, it throws an error.
--
fromInfer ::
  forall sort. (KnownSort sort, Eq (Info sort)) =>
  K Provenance sort -> Infer sort (CheckedTree sort) ->
  (Check sort (CheckedTree sort), Infer sort (CheckedTree sort))
fromInfer p inf = (inferToCheck p inf, inf)
  where
    inferToCheck ::
      forall sort. (KnownSort sort, Eq (Info sort)) =>
      K Provenance sort -> Infer sort (CheckedTree sort) -> Check sort (CheckedTree sort)
    inferToCheck p inf = Chk $ do
      (tree, inferred) <- lift (unInf inf)
      expected <- ask
      if inferred == expected
        then return tree
        else throwError (Mismatch @sort (unK p) inferred expected)

-- |Run and continue in checking mode.
runCheck :: SortedCheckInfer sort -> Check sort (CheckedTree sort)
runCheck (SCI (chk, _inf)) = chk

-- |Run and continue in inference mode.
runInfer :: SortedCheckInfer sort -> Infer sort (CheckedTree sort)
runInfer (SCI (_chk, inf)) = inf

{-

--- Roughly:
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

-- -}
-- -}
-- -}
-- -}
-- -}
