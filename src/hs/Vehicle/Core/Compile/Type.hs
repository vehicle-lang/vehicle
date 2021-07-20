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
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (MonadState(..), StateT(..), modify)
import Data.Text (Text)
import Text.Printf (printf)
import Data.List (foldl')
import Data.List.NonEmpty qualified as NonEmpty (scanl, head)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Coerce (coerce)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.List.NonEmpty (NonEmpty(..))
import Prettyprinter ( (<+>), Pretty(pretty), encloseSep, lbracket, rbracket, comma )

import Vehicle.Core.AST hiding (lift)
import Vehicle.Core.Compile.DSL
-- import Vehicle.Core.Print ( prettyInfo )
import Vehicle.Prelude
import Vehicle.Error

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
      [DeBruijnExpr Provenance] -- ^ The possible inferred types.
      (DeBruijnExpr Provenance) -- ^ The expected type.
  | MissingAnnotation
      Provenance  -- ^ The location of the missing annotation.
  | UnsupportedOperation
      Provenance  -- ^ The location of the unsupported operation.
      Text        -- ^ A description of the unsupported operation.
{-
instance MeaningfulError TypeError where
  details (IndexOutOfBounds p index) = DError $ DeveloperError
    { provenance = p
    , problem    = "DeBruijn index" <+> pretty index <+> "out of bounds"
    }

  details (Mismatch p candidates expected) = UError $ UserError
    { provenance = p
    , problem    = "expected something of type" <+> prettyInfo expected <+>
                   "but inferred types" <+> encloseSep lbracket rbracket comma (map prettyInfo candidates)
    , fix        = "unknown"
    }

  details (MissingAnnotation p) = DError $ DeveloperError
    { provenance = p
    , problem    = "missing annotation"
    }

  details (UnsupportedOperation p t) = UError $ UserError
    { provenance = p
    , problem    = "type-checking of" <+> squotes t <+> "not currently supported"
    , fix        = "unknown"
    }
-}
-- * Type and kind contexts

-- |Type context.

type TypingError = ()

type DeclCtx  = Map Ident CheckedExpr
type BoundCtx = [CheckedExpr]
data MetaCtx  = MetaCtx
  { nextMeta               :: Meta
  , metaVariableTypes      :: IntMap CheckedExpr
  , unificationConstraints :: [(CheckedExpr, CheckedExpr)]
  , typeClassConstraints   :: [CheckedExpr]
  }

type TCM a =
  StateT MetaCtx
    (ReaderT BoundCtx
      (ReaderT DeclCtx
        (Except TypingError)))
          a

getMetaCtx :: TCM MetaCtx
getMetaCtx = get

modifyMetaCtx :: (MetaCtx -> MetaCtx) -> TCM ()
modifyMetaCtx = modify

getMetaType :: Meta -> TCM CheckedExpr
getMetaType i = do
  ctx <- getMetaCtx;
  case IntMap.lookup i (metaVariableTypes ctx) of
    Just typ -> typ
    Nothing  -> developerError (printf "Meta variable ?%d not found in context" i)

getBoundCtx :: TCM BoundCtx
getBoundCtx = ask

getDeclCtx :: TCM DeclCtx
getDeclCtx = lift (lift ask)

freshMetaName :: TCM Meta
freshMetaName = do
  MetaCtx {..} <- getMetaCtx;
  put $ MetaCtx { nextMeta = succ nextMeta , ..}
  return nextMeta

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
freshMeta :: CheckedExpr -> TCM CheckedExpr
freshMeta resultType = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create a Pi type that abstracts over all bound variables
  boundCtx  <- getBoundCtx
  let metaType = foldl' (tPi Explicit) resultType boundCtx

  -- Stores type in meta-context
  modifyMetaCtx $ \MetaCtx {..} ->
    MetaCtx { metaVariableTypes = IntMap.insert metaName metaType metaVariableTypes , ..}

  -- Create bound variables for everything in the context
  let boundEnv =
        [ Bound (makeTypeAnn varType) (Index varIndex)
        | (varIndex , varType) <- zip [0..] boundCtx ]

  -- Returns a meta applied to every bound variable in the context
  let meta = foldl' app (Meta metaName) boundEnv
  return meta

freshUncheckedMeta :: CheckedExpr -> TCM UncheckedExpr
freshUncheckedMeta resultType = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create a Pi type that abstracts over all bound variables
  boundCtx  <- getBoundCtx
  let metaType = foldl' (tPi Explicit) resultType boundCtx

  -- Stores type in meta-context
  modifyMetaCtx $ \MetaCtx {..} ->
    MetaCtx { metaVariableTypes = IntMap.insert metaName metaType metaVariableTypes , ..}

  -- Create bound variables for everything in the context
  let boundEnv = [ Bound mempty (Index varIndex) | varIndex <- [0..length boundCtx - 1]]

  -- Returns a meta applied to every bound variable in the context
  let meta = foldl' (\f x -> App mempty f (Arg Explicit x)) (Meta metaName) boundEnv
  return meta

check :: CheckedExpr     -- Type we're checking against
      -> UncheckedExpr   -- Expression being type-checked
      -> TCM CheckedExpr -- Updated expression
check target = \case
  Type l             -> _
  Constraint         -> _
  Meta     _         -> _
  Ann      ann _ _   -> _
  App      ann _ _   -> _
  Pi       ann _ _   -> _
  Builtin  ann _     -> _
  Bound    ann _     -> _
  Free     ann _     -> _
  Let      ann _ _ _ -> _
  Lam      ann _ _   -> _
  Literal  ann _     -> _
  Seq      ann _     -> _

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
infer :: UncheckedExpr
      -> TCM (CheckedExpr, CheckedExpr)
infer = \case
  Type l -> return (Type l , Type (l + 1))

  Constraint -> return (Constraint , Type 1)

  Meta i -> do
    metaType <- getMetaType i
    return (Meta i , metaType)

  Ann      ann e t   -> do
    (t', _) <- infer t
    e' <- check t' e
    let ann' = RecAnn t' ann
    return (Ann ann' e' t' , t')

  App p fun (Arg argVis arg) -> do
    -- Infer the kind of the function.
    (fun' , tFun) <- infer fun

    -- Check if it's a function kind: if so, return the two argument; if not, throw an error.
    case tFun of
      Pi _ (Binder _ funVis _ tArg) tRes
        | argVis == funVis -> do
          -- Check the kind of the argument.
          arg' <- check tArg arg

          -- Return the appropriately annotated type with its inferred kind.
          return (App (RecAnn tRes p) fun' (Arg argVis arg') , tRes)

      Pi _ (Binder _ Implicit _ tArg) tRes -> do
        metaArg <- freshUncheckedMeta tArg
        let funWithImplicit = App mempty fun (Arg Implicit metaArg)
        infer (App p funWithImplicit (Arg argVis arg))

      _ -> do
        expected <- (~>) <$> freshMeta <*> freshMeta
        throwError $ Mismatch p tFun expected


  Pi       ann _ _   -> _

  Builtin  ann _     -> _

  Bound    ann _     -> _

  Free     ann _     -> _

  Let      ann _ _ _ -> _

  Lam      ann _ _   -> _

  Literal  ann _     -> _

  Seq      ann _     -> _

{-
-- |Create a context with a single piece of information of the given sort.
singletonCtx :: (sort `In` ['TYPE, 'EXPR]) => SSort sort -> Info DeBruijn sort -> Ctx
singletonCtx STYPE info = Ctx (Seq.singleton info) Seq.empty
singletonCtx SEXPR info = Ctx Seq.empty (Seq.singleton info)

-- |Get the sub-context for a given sort.
getSubCtxFor :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => Ctx -> Seq (Info DeBruijn sort)
getSubCtxFor = case sortSing @sort of STYPE -> typeInfo; SEXPR -> exprInfo


-- * Bidirectional type checking and inference algorithm

-- |The type of trees output by the type checking algorithm.
type CheckedTree (sort :: Sort) = Tree DeBruijn (Info DeBruijn :*: K Provenance) sort

-- |The type checking monad stack shared between checking and inference. It provides:
--
--    1. a dataflow monad which propagates the kind and type environment according to sort;
--    2. a supply monad which provides an infinite supply of fresh meta-variables; and
--    3. an exception monad for throwing type errors.
--
type TCM (sort :: Sort) = DataflowT sort Ctx (SupplyT Meta (Except TypeError))


-- |Type which wraps a pair of a checking and an inference monad such that they can be used
--  in a sorted position, e.g., as the recursive position in |foldTree|.
newtype SortedCheckInfer (sort :: Sort)
  = SCI { unSCI :: (Check sort (CheckedTree sort), Infer sort (CheckedTree sort)) }

-- |Find the type information for a given deBruijn index of a given sort.
getInfo ::
  forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K Provenance sort -> DeBruijn sort -> TCM sort (Info DeBruijn sort)
getInfo p db = do
  subctx <- getSubCtxFor @sort <$> ask
  let ix = toIndex db
  let maybeInfo = subctx Seq.!? ix
  maybe (throwError $ IndexOutOfBounds (unK p) ix) return maybeInfo

freshKMeta :: KnownSort sort => TCM sort (Info DeBruijn 'TYPE)
freshKMeta = Info . KMeta mempty <$> demand

freshTMeta :: KnownSort sort => TCM sort (Info DeBruijn 'EXPR)
freshTMeta = do
  x <- demand
  k <- freshKMeta
  return $ Info (TMeta (k :*: mempty) x)

-- |Check if a tree is well-kinded and well-typed, and insert typing information.
checkInfer ::
  forall sort. (KnownSort sort) =>
  Tree DeBruijn (K Provenance) sort ->
  (Check sort (CheckedTree sort), Infer sort (CheckedTree sort))
checkInfer = unSCI . foldTree (SCI . checkInferF)

-- |Check if a single layer is well-kinded and well-typed, see |checkInfer|.
checkInferF ::
  forall sort. (KnownSort sort) =>
  TreeF DeBruijn (K Provenance) sort SortedCheckInfer ->
  (Check sort (CheckedTree sort), Infer sort (CheckedTree sort))
checkInferF = case sortSing @sort of

  -- Kinds.
  SKIND -> \case
    KAppF  p k1 k2 -> fromCheck p $ KApp (mempty :*: p) <$> runCheck k1 <*> runCheck k2
    KConF  p op    -> fromCheck p $ pure (KCon (mempty :*: p) op)
    KMetaF p _i    -> fromCheck p $ throwError $ UnsupportedOperation (unK p) "KMeta"
  --
  -- TODO: convert to Hindley-Milner style checking for kind checking, so that we can generalise
  --       the forall without requiring a type annotation.
  --
  -- TODO: variable names are clashing with the DSL for writing types (e.g. tForall)
  STYPE -> \case

    -- For type quantification:
    -- the result of a quantification also has kind |kType|
    TForallF p kOptArg n t -> fromInfer p $ do
      kArg <- maybe (return kType) (fmap Info . flow . runCheckWith mempty) kOptArg
      bindLocal (runCheckWith kArg n) $ \n -> do
        t <- runCheckWith kType t
        let kRes = kType
        return (TForall (kRes :*: p) (Just $ unInfo kArg) n t, kRes)

    -- For type applications:
    -- infer the kind of the type function, check the kind of its argument.
    TAppF p tFun tArg -> fromInfer p $ do


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
      throwError $ UnsupportedOperation (unK p) "TMeta"

  -- Type argument
  STARG -> \case
    TArgF p n -> fromCheck p $ do
      k <- ask
      lift (tellData $ singletonCtx STYPE (Info . unInfo $ k))
      return (TArg (k :*: p) n)

  -- Expressions
  SEXPR -> \case
    EAnnF p e t ->
      let t' :: TCM 'EXPR (Info DeBruijn 'EXPR)
          t' = do (t, _k) <- flow (runInfer t)
                  return (Info t)
      in fromCheckWithAnn p t' (runCheck e)

    EVarF p n -> fromInfer p $ do
      t <- getInfo p n
      return (EVar (t :*: p) n, t)

    ELetF p n e1 e2 -> fromInfer p $ do
      -- Infer the type of the let body
      (e1, t1) <- runInfer e1

      -- Add the let bound name to the context and type check the body
      bindLocal (runCheckWith (coerce t1) n) $ \n -> do
        (e2, t2) <- runInfer e2
        return (ELet (t2 :*: p) n e1 e2, t2)

    ELamF p n e -> fromCheck p $ do
      tFun <- ask

      -- Check if it's a function type: if so, return the two arguments; if not, throw an error.
      (tArg, tRes) <- case unInfo tFun of
        _tFun@(TApp _ (TApp _ (TCon _ TFun) tArg) tRes) ->
          return (Info tArg, Info tRes)
        _tFun -> do
          expected <- lift $ (~>) <$> freshTMeta <*> freshTMeta
          throwError $ Mismatch (unK p) [tFun] expected

      -- Add the argument to the context and type check the body
      lift $ bindLocal (runCheckWith tArg n) $ \n -> do
        e <- runCheckWith tRes e
        return $ ELam (tRes :*: p) n e

    EAppF p eFun eArg -> fromInfer p $ do
      -- Infer the type of the function.
      (eFun, tFun) <- runInfer eFun

      -- Check if it's a function type: if so, return the two arguments; if not, throw an error.
      (tArg, tRes) <- case unInfo tFun of
        _tFun@(TApp _ (TApp _ (TCon _ TFun) tArg) tRes) ->
          return (Info tArg, Info tRes)
        _tFun -> do
          expected <- (~>) <$> freshTMeta <*> freshTMeta
          throwError $ Mismatch (unK p) [tFun] expected

      -- Check the kind of the argument.
      eArg <- runCheckWith tArg eArg

      -- Return the appropriately annotated type with its inferred kind.
      return (EApp (tRes :*: p) eFun eArg, tRes)

    ETyAppF p eTyFun tArg -> fromInfer p $ do
      (eTyFun, tForall') <- runInfer eTyFun

      -- Check if it's a forall type: if so, return the two arguments; if not, throw an error.
      (kArg, tRes) <- case unInfo tForall' of
        _tForall'@(TForall _ _ tArg tRes) ->
          return (ifst (annotation tArg) , tRes)
        _tForall' -> do
          tMeta <- freshTMeta
          kMeta <- freshKMeta
          let expected = tForall kMeta (const tMeta)
          throwError $ Mismatch (unK p) [tForall'] expected

      tArg <- flow $ runCheckWith (coerce kArg) tArg
      let tRes' = Info $ subst tArg tRes

      return (ETyApp (tRes' :*: p) eTyFun tArg, tRes')

    ETyLamF p n e -> fromCheck p $ do
      tForall' <- ask

      -- Check if it's a forall type: if so, return the two arguments; if not, throw an error.
      (kArg, tRes) <- case unInfo tForall' of
        _tForall'@(TForall _ _ tArg tRes) ->
          return (ifst (annotation tArg) , Info tRes)
        _tForall' -> do
          tMeta <- lift freshTMeta
          kMeta <- lift freshKMeta
          let expected = tForall kMeta (const tMeta)
          throwError $ Mismatch (unK p) [tForall'] expected

      -- Add the argument to the context and check the body
      lift $ bindLocal (runCheckWith kArg n) $ \n -> do
        e <- runCheckWith tRes e
        return $ ETyLam (tRes :*: p) n e

    EConF p op -> fromInfer p $ do
      let t = typeOf op
      return (ECon (t :*: p) op, t)

    ELitIntF p z -> fromInfer p $ do
      let t = tInt
      return (ELitInt (t :*: p) z, t)

    ELitRealF p r -> fromInfer p $ do
      let t = tReal
      return (ELitReal (t :*: p) r, t)

    ELitSeqF p es -> fromCheck p $ do
      tExpected <- ask
      let actualFirstDimension = toInteger $ length es

      tElem <- case unInfo tExpected of
        (TApp ann1 (TApp ann2 (TCon ann3 TTensor) tElem) (TLitDimList ann4 (TLitDim _ann5 expectedFirstDimension :| ds)))
          | actualFirstDimension == expectedFirstDimension -> case ds of
            -- If there is only one dimension, the elements' type should be the tensor's type argument.
            []         -> return $ Info tElem
            -- If there is more than one dimension, the elements's type should be a tensor with one less dimension than the original.
            (d2 : ds2) -> return $ Info $ TApp ann1 (TApp ann2 (TCon ann3 TTensor) tElem) (TLitDimList ann4 (d2 :| ds2))
        (TApp _ (TCon _ TList) tElem) ->
          return $ Info tElem
        _ -> do
          tElem <- lift freshTMeta
          let tTensorActual = tTensor tElem (tLitDimList (tLitDim actualFirstDimension :| []))
          let tListActual   = tList tElem
          throwError $ Mismatch (unK p) [tTensorActual , tListActual] tExpected

      es <- lift (traverse (runCheckWith tElem) es)
      return $ ELitSeq (tExpected :*: p) es


  -- Expression arguments
  SEARG -> \case
    EArgF p n -> fromCheck p $ do
      t <- ask
      lift (tellData $ singletonCtx SEXPR (coerce t))
      return (EArg (t :*: p) n)

  -- Declarations
  SDECL -> \case
    DeclNetwF p n t    -> fromInfer p $ do
      (t , _k) <- flow $ runInfer t
      -- BOB: should we check that the type has kind STAR?
      n <- flow $ runCheckWith (Info t) n
      return (DeclNetw (mempty :*: p) n t, mempty)

    DeclDataF p n t    -> fromInfer p $ do
      (t , _k) <- flow $ runInfer t
      n <- flow $ runCheckWith (Info t) n
      return (DeclData (mempty :*: p) n t, mempty)

    DefTypeF  p n nArgs t -> undefined

    DefFunF   p n t e  -> fromInfer p $ do
      (t , _k) <- flow $ runInfer t
      e <- flow $ runCheckWith (Info t) e
      n <- flow $ runCheckWith (Info t) n
      return (DefFun (mempty :*: p) n t e, mempty)

  -- Programs
  SPROG -> \case
    MainF p ds -> fromInfer p $ do
      ds <- flow $ traverse (runCheckWith (Info ())) ds
      return (Main (mempty :*: p) ds , Info ())


-- |Switch from inference mode to checking mode upon finding a type annotation.
fromCheckWithAnn ::
  forall sort. (KnownSort sort) =>
  K Provenance sort -> TCM sort (Info DeBruijn sort) -> Check sort (CheckedTree sort) ->
  (Check sort (CheckedTree sort), Infer sort (CheckedTree sort))
fromCheckWithAnn p tcmAnnotated chk = fromInfer p inf
  where
    inf :: Infer sort (CheckedTree sort)
    inf = do annotated <- tcmAnnotated
             tree <- runReaderT chk annotated
             return (tree, annotated)

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
    inferNoop :: forall sort a. (KnownSort sort, Monoid (Info DeBruijn sort)) => Check sort a -> Infer sort a
    inferNoop chk = do x <- runReaderT chk mempty; return (x, mempty)

-- |Constructed a bidirectional step from an inference step.
--
--  Concretely, the resulting algorithm runs the inference mode, then compares
--  the inferred type to the expected type. If they're equal, it returns as
--  usual. If they're not, it throws an error.
--
fromInfer ::
  forall sort. (KnownSort sort, Eq (Info DeBruijn sort)) =>
  K Provenance sort -> Infer sort (CheckedTree sort) ->
  (Check sort (CheckedTree sort), Infer sort (CheckedTree sort))
fromInfer p inf = (inferToCheck p inf, inf)
  where
    inferToCheck ::
      forall sort. (KnownSort sort, Eq (Info DeBruijn sort)) =>
      K Provenance sort -> Infer sort (CheckedTree sort) -> Check sort (CheckedTree sort)
    inferToCheck p inf = do
      (tree, inferred) <- lift inf
      expected <- ask
      if inferred == expected
        then return tree
        else throwError (Mismatch @sort (unK p) [inferred] expected)

-- |Run and continue in checking mode.
runCheck :: SortedCheckInfer sort -> Check sort (CheckedTree sort)
runCheck (SCI (chk, _inf)) = chk

-- |Run and continue in checking mode.
runCheckWith :: Info DeBruijn sort -> SortedCheckInfer sort -> TCM sort (CheckedTree sort)
runCheckWith info (SCI (chk, _inf)) = runReaderT chk info

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

-}
-}