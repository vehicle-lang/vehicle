module Vehicle.Core.Compile.Type where

import Control.Monad (when, unless)
import Control.Monad.Except (MonadError(..), Except)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State (MonadState(..), StateT(..), modify)
import Data.Text (Text)
import Data.Foldable (toList)
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

data TypingError
  = IndexOutOfBounds
    Provenance    -- ^ The location of the deBruijn index.
    Index         -- ^ The deBruijn index.
  | UnresolvedHole
    Provenance    -- ^ The location of the hole
    Symbol        -- ^ The name of the hole
  | Mismatch
    Provenance    -- ^ The location of the mismatch.
    CheckedExpr   -- ^ The possible inferred types.
    CheckedExpr   -- ^ The expected type.
  | MissingAnnotation
    Provenance    -- ^ The location of the missing annotation.
  | UnsupportedOperation
    Provenance    -- ^ The location of the unsupported operation.
    Text          -- ^ A description of the unsupported operation.
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
data UnificationConstraint = Unify CheckedExpr CheckedExpr
data TypeClassConstraint   = Meta `Has` CheckedExpr

type DeclCtx  = Map Symbol CheckedExpr
type BoundCtx = Seq CheckedExpr
data MetaCtx  = MetaCtx
  { nextMeta               :: Meta
  , metaVariableTypes      :: IntMap CheckedExpr
  , unificationConstraints :: [UnificationConstraint]
  , typeClassConstraints   :: [TypeClassConstraint]
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

addUnificationConstraints :: [UnificationConstraint] -> TCM ()
addUnificationConstraints cs = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { unificationConstraints = cs <> unificationConstraints, ..}

addTypeClassConstraints :: [TypeClassConstraint] -> TCM ()
addTypeClassConstraints ts = modifyMetaCtx $ \ MetaCtx {..} ->
  MetaCtx { typeClassConstraints = ts <> typeClassConstraints, ..}

getMetaType :: Meta -> TCM CheckedExpr
getMetaType i = do
  ctx <- getMetaCtx;
  case IntMap.lookup i (metaVariableTypes ctx) of
    Just typ -> return typ
    Nothing  -> developerError (printf "Meta variable ?%d not found in context" i)

getBoundCtx :: TCM BoundCtx
getBoundCtx = ask

modifyBoundCtx :: (BoundCtx -> BoundCtx) -> TCM a -> TCM a
modifyBoundCtx = local

getDeclCtx :: TCM DeclCtx
getDeclCtx = lift (lift ask)

freshMetaName :: TCM Meta
freshMetaName = do
  MetaCtx {..} <- getMetaCtx;
  put $ MetaCtx { nextMeta = succ nextMeta , ..}
  return nextMeta

-- TODO unify these functions in a pleasing way

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshMeta :: CheckedExpr -> TCM (Meta, CheckedExpr)
freshMeta resultType = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create a Pi type that abstracts over all bound variables
  boundCtx  <- getBoundCtx
  let metaType = foldl' (tPiInternal Explicit) resultType boundCtx

  -- Stores type in meta-context
  modifyMetaCtx $ \MetaCtx {..} ->
    MetaCtx { metaVariableTypes = IntMap.insert metaName metaType metaVariableTypes , ..}

  -- Create bound variables for everything in the context
  let boundEnv =
        [ Bound (makeTypeAnn varType) (Index varIndex)
        | (varIndex , varType) <- zip [0..] (toList boundCtx) ]

  -- Returns a meta applied to every bound variable in the context
  let meta = foldl' app (Meta metaName) boundEnv
  return (metaName, meta)

-- | Creates a fresh meta variable. Meta variables need to remember what was
-- in the current context when they were created. We do this by creating a
-- meta-variable that takes everything in the current context as an argument
-- and then which is immediately applied to everything in the current context.
-- Post unification, any unneeded context arguments will be normalised away.
-- It returns the name of the meta and the expression of it applied to every
-- variable in the context.
freshUncheckedMeta :: CheckedExpr -> TCM (Meta, UncheckedExpr)
freshUncheckedMeta resultType = do
  -- Create a fresh name
  metaName <- freshMetaName

  -- Create a Pi type that abstracts over all bound variables
  boundCtx  <- getBoundCtx
  let metaType = foldl' (tPiInternal Explicit) resultType boundCtx

  -- Stores type in meta-context
  modifyMetaCtx $ \MetaCtx {..} ->
    MetaCtx { metaVariableTypes = IntMap.insert metaName metaType metaVariableTypes , ..}

  -- Create bound variables for everything in the context
  let boundEnv = [ Bound mempty (Index varIndex) | varIndex <- [0..length boundCtx - 1]]

  -- Returns a meta applied to every bound variable in the context
  let meta = foldl' (\f x -> App mempty f (Arg mempty Explicit x)) (Meta metaName) boundEnv
  return (metaName, meta)

check :: CheckedExpr     -- Type we're checking against
      -> UncheckedExpr   -- Expression being type-checked
      -> TCM CheckedExpr -- Updated expression
check target = _
{-\case
  Type l             -> _
  Constraint         -> _
  Meta     _         -> _
  Hole     _ _       -> _
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
-}
{-

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
-}
    {-
    ELamF p n e -> fromCheck p $ do
      tFun <- ask

      -- Check if it's a function type: if so, return the two arguments; if not, throw an error.
      (tArg, tRes) <- case unInfo tFun of
        _tFun@(TApp _ (TApp _ (TCon _ TFun) tArg) tRes) ->
          return (Info tArg, Info tRes)
        _tFun -> do
          expected <- lift $ (~>) <$> freshTMeta <*> freshTMeta
          throwError $ Mismatch (unK p) [tFun] expected

      -}

-- | Takes in an unchecked expression and attempts to infer it's type.
-- Returns the expression annotated with its type as well as the type itself.
infer :: UncheckedExpr
      -> TCM (CheckedExpr, CheckedExpr)
infer = \case
  Type l ->
    return (Type l , Type (l + 1))

  Constraint ->
    return (Constraint , Type 1)

  Meta i -> do
    metaType <- getMetaType i
    return (Meta i , metaType)

  Hole p s ->
    throwError $ UnresolvedHole p s

  Ann      ann e t   -> do
    (t', _) <- infer t
    e' <- check t' e
    let ann' = RecAnn t' ann
    return (Ann ann' e' t' , t')

  App p fun arg@(Arg pArg vArg eArg) -> do
    -- Infer the type of the function.
    (fun' , tFun') <- infer fun

    case tFun' of
      -- Check if it's a Pi type which is expecting the right visibility
      Pi _ (Binder _ vFun _ tArg) tRes
        | vArg == vFun -> do
          -- Check the type of the argument.
          eArg' <- check tArg eArg

          -- Return the appropriately annotated type with its inferred kind.
          return (App (RecAnn tRes p) fun' (Arg pArg vArg eArg') , tRes)

      -- Check if it's a Pi type which is expecting an implicit argument
      -- but is being applied to an explicit argument
      Pi _ (Binder _ Implicit _ tArg) _tRes -> do
        -- Generate a fresh meta variable
        (meta, metaArg) <- freshUncheckedMeta tArg

        -- TODO Wen is worried about interactions between the Pi abstractions over
        -- the context and the type-class search later on.

        -- Check if the implicit argument is a type-class
        when (isConstraint tArg) $
          addTypeClassConstraints [meta `Has` tArg]

        -- Apply the function to the new meta.
        let funWithImplicit = App mempty fun (Arg mempty Implicit metaArg)

        -- Try again to infer the type of the application.
        infer (App p funWithImplicit arg)

      _ -> do
        let expected = hole "a" ~> hole "b"
        throwError $ Mismatch p tFun' expected


  Pi p (Binder pBound vis name arg) res -> do
    (arg', tArg') <- infer arg

    modifyBoundCtx (arg' Seq.<|) $ do
      (res', tRes') <- infer res
      let t' = tArg' `tMax` tRes'
      return (Pi (RecAnn t' p) (Binder pBound vis name arg') res' , t')

  Builtin p op -> do
    let t' = typeOfBuiltin p op
    return (Builtin (RecAnn t' p) op, t')

  Bound p (Index i) -> do
    -- Lookup the type of the variable in the context.
    ctx <- getBoundCtx
    case ctx Seq.!? i of
      Just t' -> return (Bound (RecAnn t' p) (Index i), t')
      Nothing -> developerError $
        printf "Index %d out of bounds when looking up variable in context %s at %s" i (show ctx) (showProv p)

  Free p name -> do
    -- Lookup the type of the declaration variable in the context.
    ctx <- getDeclCtx
    case Map.lookup name ctx of
      Just t' -> return (Free (RecAnn t' p) name, t')
      -- This should have been caught during scope checking
      Nothing -> developerError $
        printf "Declaration %s not found when looking up variable in context %s at %s" name (show ctx) (showProv p)

  Let p (Binder pBound vis name tBound) arg body -> do
    -- Infer the type of the let arg from the annotation on the binder
    (tBound', _) <- infer tBound

    -- Check the arg actually has that type
    arg' <- check tBound' arg

    -- Update the context with the bound variable
    modifyBoundCtx (tBound' Seq.<|) $ do
      -- Infer the type of the body
      (body' , tBody') <- infer body
      return (Let (RecAnn tBody' p) (Binder pBound vis name tBound') arg' body' , tBody')

  Lam p (Binder pBound vis name tBound) body -> do
    -- Infer the type of the bound variable from the binder
    (tBound', _) <- infer tBound

    -- Update the context with the bound variable
    modifyBoundCtx (tBound' Seq.<|) $ do
      (body' , tBody') <- infer body
      let t' = tPi pBound vis name tBound' tBody'
      return (Lam (RecAnn t' p) (Binder pBound vis name t') body' , t')

  Literal p l -> do
    let t' = typeOfLiteral p l
    return (Literal (RecAnn t' p) l, t')

  Seq p es -> do
    (es', ts') <- unzip <$> traverse infer es
    let tElemTCM
          | null ts'  = snd <$> freshMeta Type0
          | otherwise = do
              addUnificationConstraints (zipWith Unify ts' (tail ts'))
              return $ head ts'
    tElem' <- tElemTCM

    (meta, tCont') <- freshMeta Type0
    addTypeClassConstraints [meta `Has` isContainer p tCont' tElem']

    return (Seq (RecAnn tCont' p) es' , tCont')
{-

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

    -- For type builtins:
    -- lookup the kind of the builtin using |kindOf|.
    TConF p op -> fromInfer p $ do
      let k = kindOf op
      return (TCon (k :*: p) op, k)

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

    ELitIntF p z -> fromInfer p $ do
      let t = tInt
      return (ELitInt (t :*: p) z, t)

    ELitRealF p r -> fromInfer p $ do
      let t = tReal
      return (ELitReal (t :*: p) r, t)




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

typeOfLiteral :: Provenance -> Literal -> CheckedExpr
typeOfLiteral p (LNat  _) = tForall Type0 $ \t -> isNatural p t ~~> t
typeOfLiteral p (LInt  _) = tForall Type0 $ \t -> isIntegral p t ~~> t
typeOfLiteral p (LReal _) = tForall Type0 $ \t -> isReal p t ~~> t
typeOfLiteral p (LBool _) = tForall Type0 $ \t -> isTruth p t ~~> t

-- |Return the kind for builtin exprs.
typeOfBuiltin :: Provenance -> Builtin -> CheckedExpr
typeOfBuiltin p = \case
  PrimitiveType _ -> Type0
  List            -> Type0 ~> Type0
  Tensor          -> Type0 ~> tList tNat ~> Type0

  Implements HasEq          -> Type0 ~> Type0 ~> Constraint
  Implements HasOrd         -> Type0 ~> Type0 ~> Constraint
  Implements IsTruth        -> Type0 ~> Constraint
  Implements IsNatural      -> Type0 ~> Constraint
  Implements IsIntegral     -> Type0 ~> Constraint
  Implements IsRational     -> Type0 ~> Constraint
  Implements IsReal         -> Type0 ~> Constraint
  Implements IsContainer    -> Type0 ~> Constraint
  Implements IsQuantifiable -> Type0 ~> Type0 ~> Constraint

  If   -> tForall Type0 $ \t -> tProp ~> t ~> t
  Cons -> tForall Type0 $ \t -> t ~> tList t ~> tList t

  Impl -> typeOfBoolOp2 p
  And  -> typeOfBoolOp2 p
  Or   -> typeOfBoolOp2 p
  Not  -> typeOfBoolOp1 p

  Eq   -> typeOfEqualityOp p
  Neq  -> typeOfEqualityOp p

  Le   -> typeOfComparisonOp p
  Lt   -> typeOfComparisonOp p
  Ge   -> typeOfComparisonOp p
  Gt   -> typeOfComparisonOp p

  Add  -> typeOfNumOp2 (isNatural  p)
  Sub  -> typeOfNumOp2 (isIntegral p)
  Mul  -> typeOfNumOp2 (isNatural  p)
  Div  -> typeOfNumOp2 (isRational p)
  Neg  -> typeOfNumOp1 (isIntegral p)

  At   -> typeOfAtOp p

  All  -> typeOfQuantifierOp p
  Any  -> typeOfQuantifierOp p

typeOfEqualityOp :: Provenance -> CheckedExpr
typeOfEqualityOp p =
  tForall Type0 $ \t ->
    tForall Type0 $ \r ->
      hasEq p t r ~~> t ~> t ~> r

typeOfComparisonOp :: Provenance -> CheckedExpr
typeOfComparisonOp p =
  tForall Type0 $ \t ->
    tForall Type0 $ \r ->
      hasOrd p t r ~~> t ~> t ~> r

typeOfBoolOp2 :: Provenance -> CheckedExpr
typeOfBoolOp2 p =
  tForall Type0 $ \t ->
    isTruth p t ~~> t ~> t ~> t

typeOfBoolOp1 :: Provenance -> CheckedExpr
typeOfBoolOp1 p =
  tForall Type0 $ \t ->
    isTruth p t ~~> t ~> t

typeOfNumOp2 :: (CheckedExpr -> CheckedExpr) -> CheckedExpr
typeOfNumOp2 numConstraint =
  tForall Type0 $ \t ->
    numConstraint t ~~> t ~> t ~> t

typeOfNumOp1 :: (CheckedExpr -> CheckedExpr) -> CheckedExpr
typeOfNumOp1 numConstraint =
  tForall Type0 $ \t ->
    numConstraint t ~~> t ~> t

typeOfQuantifierOp :: Provenance -> CheckedExpr
typeOfQuantifierOp p =
  tForall Type0 $ \t ->
    tForall Type0 $ \r ->
      isQuantifiable p t r ~~> t ~> (t ~> r) ~> r

typeOfAtOp :: Provenance -> CheckedExpr
typeOfAtOp p =
  tForall Type0 $ \tCont ->
    tForall Type0 $ \tElem ->
      isContainer p tCont tElem ~~> tCont ~> tNat ~> tElem