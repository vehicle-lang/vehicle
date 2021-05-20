{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE ImportQualifiedPost       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
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
import Control.Monad.Supply (SupplyT, demand)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Writer (MonadWriter(..))
import Data.Text (Text)
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
      Text        -- ^ A description of the unsupported operation.
      Provenance  -- ^ The location of the unsupported operation.

-- * Meta-variables

-- TODO: should be migrated to an AST module

type Meta = Integer

-- * Type and kind contexts

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


-- * Bidirectional type checking and inference algorithm

-- |The type of trees output by the type checking algorithm.
type CheckedTree (sort :: Sort) = Tree DeBruijn Builtin (Info :*: K Provenance) sort

-- |The type checking monad stack shared between checking and inference. It provides:
--
--    1. a dataflow monad which propagates the kind and type environment according to sort;
--    2. a supply monad which provides an infinite supply of fresh meta-variables; and
--    3. an exception monad for throwing type errors.
--
type TCM (sort :: Sort) = DataflowT sort Ctx (SupplyT Meta (Except TypeError))

-- |The checking monad, which augments the monad stack with a reader monad, which provides
--  the type to check against.
type Check (sort :: Sort) (a :: *) = ReaderT (Info sort) (TCM sort) a

-- |The inference pseudo-monad, which is exactly the type checking monad, but requires that
--  the return type is a pair of the return value and the inferred information.
--
--  Note that |Infer sort| does not form a monad, as there is no way to know how to combine
--  the various bits of information. We cannot use a writer monad, as we do not have a
--  sensible monoid instance for |Info sort|, and we want to change the manner of combining
--  these bits of information for each inference steep.
--
type Infer (sort :: Sort) (a :: *) = TCM sort (a, Info sort)

-- |Type which wraps a pair of a checking and an inference monad such that they can be used
--  in a sorted position, e.g., as the recursive position in |foldTree|.
newtype SortedCheckInfer (sort :: Sort)
  = SCI { unSCI :: (Check sort (CheckedTree sort), Infer sort (CheckedTree sort)) }

-- |Find the type information for a given deBruijn index of a given sort.
getInfo ::
  forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K Provenance sort -> DeBruijn sort -> TCM sort (Info sort)
getInfo p db = do
  subctx <- getSubCtxFor @sort <$> ask
  let ix = toIndex db
  let maybeInfo = subctx !? ix
  info <- maybe (throwError $ IndexOutOfBounds (unK p) ix) return maybeInfo
  return info

freshMeta :: KnownSort sort => TCM sort (Info 'TYPE)
freshMeta = Info . KMeta mempty <$> demand

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

  -- Kinds.
  SKIND -> \case
    KAppF  p k1 k2 -> fromCheck p $ KApp (mempty :*: p) <$> runCheck k1 <*> runCheck k2
    KConF  p op    -> fromCheck p $ KCon (mempty :*: p) <$> pure op
    KMetaF p _i    -> fromCheck p $ throwError $ UnsupportedOperation "KMeta" (unK p)

  -- Types.
  --
  -- TODO: convert to Hindley-Milner style checking for kind checking, so that we can generalise
  --       the forall without requiring a type annotation.
  --
  STYPE -> \case

    -- For type quantification:
    -- the result of a quantification also has kind |kType|
    TForallF p n t -> fromInfer p $ do
      let kArg = kType
      bindLocal (runCheckWith kArg n) $ \n -> do
        t <- runCheckWith kType t
        let kRes = kType
        return (TForall (kRes :*: p) n t, kRes)

    -- For type applications:
    -- infer the kind of the type function, check the kind of its argument.
    TAppF p tFun tArg -> fromInfer p $ do

      -- Infer the kind of the function.
      (tFun, kFun) <- runInfer tFun

      -- Check if it's a function kind: if so, return the two argument; if not, throw an error.
      (kArg, kRes) <- case unInfo kFun of
        _kFun@(KApp _ (KApp _ (KCon _ KFun) kArg) kRes) ->
          return (Info kArg, Info kRes)
        _kFun -> do
          expected <- (~>) <$> freshMeta <*> freshMeta
          throwError $ Mismatch (unK p) kFun expected

      -- Check the kind of the argument.
      tArg <- runCheckWith kArg tArg

      -- Return the appropriately annotated type with its inferred kind.
      return (TApp (kRes :*: p) tFun tArg, kRes)

    -- For type variables:
    -- lookup the kind of the variable in the context.
    TVarF p n -> fromInfer p $ do
      k <- getInfo p n
      return (TVar (k :*: p) n, k)

    -- For type builtins:
    -- lookup the kind of the builtin using |kindOf|.
    TConF p op -> fromInfer p $ do
      let k = kindOf op
      return (TCon (k :*: p) op, k)

    -- For dimension literals:
    -- all dimension literals have kind |kDim|.
    TLitDimF p d -> fromInfer p $ do
      let k = kDim
      return (TLitDim (k :*: p) d, k)

    -- For lists of dimension literals:
    -- check that each element has kind |kDim|, return |kDimList|.
    TLitDimListF p ts -> fromInfer p $ do
      ts <- traverse (runCheckWith kDim) ts
      let k = kDimList
      return (TLitDimList (k :*: p) ts, k)

    -- Type meta-variables are currently unsupported.
    TMetaF p _i -> fromCheck p $
      throwError $ UnsupportedOperation "TMeta" (unK p)

  -- Type argument
  STARG -> \case
    TArgF p n -> fromCheck p $ do
      k <- ask
      tell $ singletonCtx STYPE (fromKind . toKind $ k)
      return (TArg (k :*: p) n)

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
    inferError p = throwError $ MissingAnnotation (unK p)

    -- |For sorts with trivial information, checking and inference coincide into a no-op.
    inferNoop :: forall sort a. (KnownSort sort, Monoid (Info sort)) => Check sort a -> Infer sort a
    inferNoop chk = do x <- runReaderT chk mempty; return (x, mempty)

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
    inferToCheck p inf = do
      (tree, inferred) <- lift inf
      expected <- ask
      if inferred == expected
        then return tree
        else throwError (Mismatch @sort (unK p) inferred expected)

-- |Run and continue in checking mode.
runCheck :: SortedCheckInfer sort -> Check sort (CheckedTree sort)
runCheck (SCI (chk, _inf)) = chk

-- |Run and continue in checking mode.
runCheckWith :: Info sort -> SortedCheckInfer sort -> TCM sort (CheckedTree sort)
runCheckWith info (SCI (chk, _inf)) = runReaderT chk info

-- |Run and continue in inference mode.
runInfer :: SortedCheckInfer sort -> Infer sort (CheckedTree sort)
runInfer (SCI (_chk, inf)) = inf

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


{-

--- Roughly:
--
-- - Introduction forms are checked
-- - Elimination forms and variables are inferred
-- - if_then_else can be checkable
-- - variable with polymorphic type
-- - union-find algorithm for unification and substitution

-- -}
-- -}
-- -}
-- -}
-- -}
