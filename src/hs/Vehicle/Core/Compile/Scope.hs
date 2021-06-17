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

module Vehicle.Core.Compile.Scope
  ( checkScope
  , ScopeError(..)
  ) where

import           Control.Monad.Except
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Vehicle.Core.AST
import           Vehicle.Core.Compile.Dataflow
import           Vehicle.Prelude


-- * Errors thrown during scope checking.

-- |Type of errors thrown by scope checking.
data ScopeError = UnboundName Symbol Provenance
  deriving (Show)

-- |Throw an |UnboundName| error using an arbitrary token.
unboundName :: MonadError ScopeError m => Symbol -> Provenance -> m a
unboundName n p = throwError $ UnboundName n p


-- * Scope checking contexts.

-- |Type of scope checking contexts.
data Ctx = Ctx { typeSymbols :: Seq Symbol, exprSymbols :: Seq Symbol }

instance Semigroup Ctx where
  Ctx ts1 es1 <> Ctx ts2 es2 = Ctx (ts1 <> ts2) (es1 <> es2)

instance Monoid Ctx where
  mempty = Ctx mempty mempty

-- |Create a context with a single symbol of the given sort.
singletonCtx :: (sort `In` ['TYPE, 'EXPR]) => SSort sort -> Symbol -> Ctx
singletonCtx STYPE symbol = Ctx (Seq.singleton symbol) Seq.empty
singletonCtx SEXPR symbol = Ctx Seq.empty (Seq.singleton symbol)

-- |Get the sub-context for a given sort.
getSubCtxFor :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => Ctx -> Seq Symbol
getSubCtxFor = case sortSing @sort of STYPE -> typeSymbols; SEXPR -> exprSymbols

-- |Find the index for a given name of a given sort.
getIndex ::
  (KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K Provenance sort -> K Symbol sort -> DataflowT sort Ctx (Except ScopeError) (DeBruijn sort)
getIndex ann (K n :: K name sort) = do
  subctx <- getSubCtxFor @sort <$> askData
  let maybeIndex = Seq.elemIndexL n subctx
  let indexOrError = maybe (unboundName n (unK ann)) return maybeIndex
  fromIndex <$> indexOrError



-- * Scope checking.

-- |Check if a tree is well-scoped, replacing name tokens with deBruijn indices.
checkScope ::
  forall sort.
  (KnownSort sort) =>
  Tree (K Symbol) (K Provenance) sort ->
  Except ScopeError (Tree DeBruijn (K Provenance) sort)
checkScope = evalDataflowT mempty . unSDF . foldTree (SDF . checkScopeF)

-- |Check if a single layer is well-scoped in the appropriate data-flow context.
checkScopeF ::
  forall sort.
  (KnownSort sort) =>
  TreeF (K Symbol) (K Provenance) sort (SortedDataflowT Ctx (Except ScopeError) (Tree DeBruijn (K Provenance))) ->
         -- ^^ --                                                          -- ^^ --
  DataflowT sort Ctx (Except ScopeError) (Tree DeBruijn (K Provenance) sort)

checkScopeF = case sortSing @sort of

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
    TForallF     ann n t   -> sbindLocal n $ \n' -> TForall ann n' <$> sflow t
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
    DeclNetwF ann n t    -> do n' <- sflow n
                               t' <- sflow t
                               return $ DeclNetw ann n' t'
    DeclDataF ann n t    -> do n' <- sflow n
                               t' <- sflow t
                               return $ DeclData ann n' t'
    DefTypeF  ann n ns t -> do n' <- sflow n
                               flow $
                                 sbindAllLocal ns $ \ns' ->
                                   DefType ann n' ns' <$> unSDF t
    DefFunF   ann n t e  -> do t' <- sflow t
                               n' <- sflow n
                               e' <- sflow e
                               return $ DefFun ann n' t' e'

  -- Programs
  SPROG -> \case
    MainF ann ds -> Main ann <$> traverse sflow ds