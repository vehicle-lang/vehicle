{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# LANGUAGE ExistentialQuantification #-}
module Vehicle.Core.Compile.Scope
  ( symbolToDeBruijn
  , deBruijnToSymbol
  , ScopeError(..)
  ) where

import Control.Monad.Except
import Control.Monad.Supply (Supply, SupplyT, demand)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Prettyprinter (pretty, (<+>))

import Vehicle.Core.AST
import Vehicle.Core.Compile.Dataflow
import Vehicle.Prelude
import Vehicle.Error


-- * Errors thrown during scope checking.

-- |Type of errors thrown by scope checking.
data ScopeError
  = UnboundName Symbol Provenance
  | IndexOutOfBounds Index Int Provenance

instance MeaningfulError ScopeError where
  details  (UnboundName name p) = UError $ UserError
    { problem    = "The name" <+> squotes name <+> "is not in scope"
    , provenance = p
    -- TODO can use Levenschtein distance to search contexts/builtins
    , fix        = pretty ("Unknown" :: String)
    }

  details (IndexOutOfBounds index ctxSize p) = DError $ DeveloperError
    { problem    = "DeBruijn index" <+> pretty index <+>
                   "greater than current context size" <+> pretty ctxSize
    , provenance = p
    }

-- |Throw an |UnboundName| error using an arbitrary token.
unboundName :: MonadError ScopeError m => Symbol -> Provenance -> m a
unboundName n p = throwError $ UnboundName n p

-- |Throw an |IndexOutOfBounds| error using an arbitrary index.
indexOutOfBounds :: MonadError ScopeError m => Index -> Int -> Provenance -> m a
indexOutOfBounds index ctxSize p = throwError $ IndexOutOfBounds index ctxSize p

-- * Scope checking contexts.

-- |Type of scope checking contexts.
data Ctx a = Ctx { typeSymbols :: Seq a, exprSymbols :: Seq a }

instance Semigroup (Ctx a) where
  Ctx ts1 es1 <> Ctx ts2 es2 = Ctx (ts1 <> ts2) (es1 <> es2)

instance Monoid (Ctx a) where
  mempty = Ctx mempty mempty

-- |Create a context with a single symbol of the given sort.
singletonCtx :: (sort `In` ['TYPE, 'EXPR]) => SSort sort -> a -> Ctx a
singletonCtx STYPE symbol = Ctx (Seq.singleton symbol) Seq.empty
singletonCtx SEXPR symbol = Ctx Seq.empty (Seq.singleton symbol)

-- |Get the sub-context for a given sort.
getSubCtxFor :: forall sort a. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => Ctx a -> Seq a
getSubCtxFor = case sortSing @sort of STYPE -> typeSymbols; SEXPR -> exprSymbols

-- |Find the index for a given name of a given sort.
getIndex ::
  (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K Provenance sort ->
  K Symbol sort ->
  DataflowT sort (Ctx Symbol) (Except ScopeError) (DeBruijn sort)
getIndex ann (K n :: K name sort) = do
  subctx <- getSubCtxFor @sort <$> askData
  let maybeIndex = Seq.elemIndexL n subctx
  let indexOrError = maybe (unboundName n (prov ann)) return maybeIndex
  fromIndex <$> indexOrError

-- |Find the name for a given index of a given sort.
getName ::
  (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  Provenance ->
  DeBruijn sort ->
  DataflowT sort (Ctx Name) (Except ScopeError) (K Name sort)
getName p (db :: DeBruijn sort) = do
  subctx <- getSubCtxFor @sort <$> askData
  let index = toIndex db
  let maybeSymbol = subctx Seq.!? index
  let symbolOrError = maybe (indexOutOfBounds index (length subctx) p) return maybeSymbol
  K <$> symbolOrError

-- * Scope checking.

-- |Check if a tree is well-scoped, replacing name tokens with deBruijn indices.
symbolToDeBruijn ::
  forall sort.
  (KnownSort sort) =>
  Tree (K Symbol) (K Provenance) sort ->
  Except ScopeError (Tree DeBruijn (K Provenance) sort)
symbolToDeBruijn = evalDataflowT mempty . unSDF . foldTree (SDF . symbolToDeBruijnF)

-- |Check if a single layer is well-scoped in the appropriate data-flow context.
symbolToDeBruijnF ::
  forall sort.
  (KnownSort sort) =>
  TreeF (K Symbol) (K Provenance) sort (SortedDataflowT (Ctx Symbol) (Except ScopeError) (Tree DeBruijn (K Provenance))) ->
  DataflowT sort (Ctx Symbol) (Except ScopeError) (Tree DeBruijn (K Provenance) sort)

symbolToDeBruijnF = case sortSing @sort of

  -- Kinds
  --
  -- Kinds are always well-scoped,
  -- so the result for scope checking kinds is pure,
  -- and we simply reconstruct the type.
  --
  SKIND -> \case
    KAppF  ann k1 k2 -> KApp ann <$> sflow k1 <*> sflow k2
    KConF  ann op    -> return $ KCon ann op
    KMetaF ann i     -> return $ KMeta ann i

  -- Types
  --
  -- For binders, we add the name to the context as we recurse.
  -- For variables, we get the DeBruijn index as the position in the appropriate context.
  -- Otherwise, we simply recurse.
  --
  STYPE -> \case
    TForallF     ann k n t -> sbindLocal n $ \n' -> TForall ann <$> traverse sflow k  <*> pure n' <*> sflow t
    TAppF        ann t1 t2 -> TApp ann <$> sflow t1 <*> sflow t2
    TVarF        ann n     -> TVar ann <$> getIndex ann n
    TConF        ann op    -> return $ TCon ann op
    TLitDimF     ann d     -> return $ TLitDim ann d
    TLitDimListF ann ts    -> TLitDimList ann <$> traverse sflow ts
    TMetaF       ann i     -> return $ TMeta ann i

  -- Type arguments
  --
  -- Return the argument as-is paired with its underlying symbol.
  --
  STARG -> \case
    TArgF ann n -> do let s = unK n
                      tellData (singletonCtx STYPE s)
                      return $ TArg ann (fromSymbol s)

  -- Expressions
  --
  -- For binders, we add the name to the context as we recurse.
  -- For variables, we get the DeBruijn index as the position in the appropriate context.
  -- Otherwise, we simply recurse.
  --
  SEXPR -> \case
    EAnnF     ann e t     -> EAnn ann <$> sflow e <*> sflow t
    ELetF     ann n e1 e2 -> sbindLocal n $ \n' -> ELet ann n' <$> sflow e1 <*> sflow e2
    ELamF     ann n e     -> sbindLocal n $ \n' -> ELam ann n' <$> sflow e
    EAppF     ann e1 e2   -> EApp ann <$> sflow e1 <*> sflow e2
    EVarF     ann n       -> EVar ann <$> getIndex ann n
    ETyAppF   ann e t     -> ETyApp ann <$> sflow e <*> sflow t
    ETyLamF   ann n e     -> sbindLocal n $ \n' -> ETyLam ann n' <$> sflow e
    EConF     ann op      -> return $ ECon ann op
    ELitIntF  ann z       -> return $ ELitInt ann z
    ELitRealF ann r       -> return $ ELitReal ann r
    ELitSeqF  ann es      -> ELitSeq ann <$> traverse sflow es

  -- Expression arguments
  --
  -- Return the argument as-is paired with its underlying symbol.
  --
  SEARG -> \case
    EArgF ann n -> do let s = unK n
                      tellData (singletonCtx SEXPR s)
                      return $ EArg ann (fromSymbol s)

  -- Declarations
  SDECL -> \case
    DeclNetwF ann n t    -> do t' <- sflow t
                               n' <- sflow n
                               return $ DeclNetw ann n' t'
    DeclDataF ann n t    -> do t' <- sflow t
                               n' <- sflow n
                               return $ DeclData ann n' t'
    DefTypeF  ann n ns t -> do n' <- sflow n
                               flow $
                                 sbindAllLocal ns $ \ns' ->
                                   DefType ann n' ns' <$> unSDF t
    DefFunF   ann n t e  -> do t' <- sflow t
                               e' <- sflow e
                               n' <- sflow n
                               return $ DefFun ann n' t' e'

  -- Programs
  SPROG -> \case
    MainF ann ds -> Main ann <$> flow (traverse unSDF ds)





deBruijnToName ::
  forall sort.
  (KnownSort sort) =>
  Tree DeBruijn (Info (K Symbol) :*: K Provenance) sort ->
  Except ScopeError (Tree (K Name) (Info (K Symbol) :*: K Provenance) sort)
deBruijnToName = evalDataflowT mempty . unSDF . foldTree (SDF . deBruijnToNameF)

-- |Check if a single layer is well-scoped in the appropriate data-flow context.
deBruijnToNameF ::
  forall sort.
  (KnownSort sort) =>
  TreeF DeBruijn (Info (K Symbol) :*: K Provenance) sort (SortedDataflowT (Ctx Name) (Except ScopeError) (Tree (K Name) (Info (K Symbol) :*: K Provenance))) ->
  DataflowT sort (Ctx Name) (Except ScopeError) (Tree (K Name) (Info (K Symbol) :*: K Provenance) sort)

deBruijnToNameF = case sortSing @sort of

  -- Kinds
  --
  -- Kinds are always well-scoped,
  -- so the result for scope checking kinds is pure,
  -- and we simply reconstruct the type.
  --
  SKIND -> \case
    KAppF  ann k1 k2 -> KApp ann <$> sflow k1 <*> sflow k2
    KConF  ann op    -> return $ KCon ann op
    KMetaF ann i     -> return $ KMeta ann i

  -- Types
  --
  -- For binders, we add the name to the context as we recurse.
  -- For variables, we get the name from the position in the appropriate context.
  -- Otherwise, we simply recurse.
  --
  STYPE -> \case
    TForallF     ann k n t -> sbindLocal n $ \n' -> TForall ann <$> traverse sflow k <*> pure n' <*> sflow t
    TAppF        ann t1 t2 -> TApp ann <$> sflow t1 <*> sflow t2
    TVarF        ann n     -> TVar ann <$> getName (prov ann) n
    TConF        ann op    -> return $ TCon ann op
    TLitDimF     ann d     -> return $ TLitDim ann d
    TLitDimListF ann ts    -> TLitDimList ann <$> traverse sflow ts
    TMetaF       ann i     -> return $ TMeta ann i

  -- Type arguments
  --
  -- Return the argument as-is paired with its underlying symbol.
  --
  STARG -> \case
    TArgF ann n -> do let s = toName n
                      tellData (singletonCtx STYPE s)
                      return $ TArg ann (K s)

  -- Expressions
  --
  -- For binders, we add the name to the context as we recurse.
  -- For variables, we get the name from the position in the appropriate context.
  -- Otherwise, we simply recurse.
  --
  SEXPR -> \case
    EAnnF     ann e t     -> EAnn ann <$> sflow e <*> sflow t
    ELetF     ann n e1 e2 -> sbindLocal n $ \n' -> ELet ann n' <$> sflow e1 <*> sflow e2
    ELamF     ann n e     -> sbindLocal n $ \n' -> ELam ann n' <$> sflow e
    EAppF     ann e1 e2   -> EApp ann <$> sflow e1 <*> sflow e2
    EVarF     ann n       -> EVar ann <$> getName (prov ann) n
    ETyAppF   ann e t     -> ETyApp ann <$> sflow e <*> sflow t
    ETyLamF   ann n e     -> sbindLocal n $ \n' -> ETyLam ann n' <$> sflow e
    EConF     ann op      -> return $ ECon ann op
    ELitIntF  ann z       -> return $ ELitInt ann z
    ELitRealF ann r       -> return $ ELitReal ann r
    ELitSeqF  ann es      -> ELitSeq ann <$> traverse sflow es

  -- Expression arguments
  --
  -- Return the argument as-is paired with its underlying symbol.
  --
  SEARG -> \case
    EArgF ann n -> do let s = toName n
                      tellData (singletonCtx SEXPR s)
                      return $ EArg ann (K s)

  -- Declarations
  SDECL -> \case
    DeclNetwF ann n t    -> do t' <- sflow t
                               n' <- sflow n
                               return $ DeclNetw ann n' t'
    DeclDataF ann n t    -> do t' <- sflow t
                               n' <- sflow n
                               return $ DeclData ann n' t'
    DefTypeF  ann n ns t -> do n' <- sflow n
                               flow $
                                 sbindAllLocal ns $ \ns' ->
                                   DefType ann n' ns' <$> unSDF t
    DefFunF   ann n t e  -> do t' <- sflow t
                               e' <- sflow e
                               n' <- sflow n
                               return $ DefFun ann n' t' e'

  -- Programs
  SPROG -> \case
    MainF ann ds -> Main ann <$> flow (traverse unSDF ds)

-- | Maps any machine (i.e. automatically generated) names to symbols
-- provided by the supply monad.
nameToSymbol :: forall ann sort.
  (KnownSort sort) =>
  Tree (K Name) ann sort ->
  Supply Symbol (Tree (K Symbol) ann sort)
nameToSymbol = traverseTreeName (go . unK >=> return . K)
  where
    go :: Name -> Supply Symbol Symbol
    go (User symbol) = return symbol
    go Machine       = demand


-- | Converts deBruijn indices back into symbols with an machine
-- (i.e. automatically generated) names provided by the supply monad.
deBruijnToSymbol ::
  forall sort.
  (KnownSort sort) =>
  Tree DeBruijn (Info DeBruijn :*: K Provenance) sort ->
  ExceptT ScopeError (SupplyT Symbol Identity) (Tree (K Symbol) (Info (K Symbol) :*: K Provenance) sort)
deBruijnToSymbol t = traverseTreeAnn convertAnn t >>= convertLayer
  where
    convertLayer ::
      forall sort1.
      (KnownSort sort1) =>
      Tree DeBruijn (Info (K Symbol) :*: K Provenance) sort1 ->
      ExceptT ScopeError (SupplyT Symbol Identity) (Tree (K Symbol) (Info (K Symbol) :*: K Provenance) sort1)
    convertLayer t = do
      ct <- mapExceptT (return . runIdentity) (deBruijnToName t)
      lift (nameToSymbol ct)

    convertAnn ::
      forall sort2.
      KnownSort sort2 =>
      (Info DeBruijn :*: K Provenance) sort2 ->
      ExceptT ScopeError (SupplyT Symbol Identity) ((Info (K Symbol) :*: K Provenance) sort2)
    convertAnn (Info v :*: p) = case sortSing @sort2 of
      SKIND -> return $ Info v :*: p
      STYPE -> do v <- deBruijnToSymbol v; return $ Info v :*: p
      STARG -> do v <- deBruijnToSymbol v; return $ Info v :*: p
      SEXPR -> do v <- deBruijnToSymbol v; return $ Info v :*: p
      SEARG -> do v <- deBruijnToSymbol v; return $ Info v :*: p
      SDECL -> return $ Info v :*: p
      SPROG -> return $ Info v :*: p