{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Vehicle.Core.Compile.Scope
  ( checkScope
  , ScopeError(..)
  ) where

import           Control.Monad.Except (MonadError(..), Except)
import           Control.Monad.Reader (MonadReader(..), ReaderT)
import           Control.Monad.State (StateT)
import           Control.Monad.Writer (MonadWriter(..))
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List (elemIndex)
import           Vehicle.Core.AST
import           Vehicle.Core.Compile.DataFlow as DF
import           Vehicle.Prelude


-- * Errors thrown during scope checking.

-- |Type of errors thrown by scope checking.
newtype ScopeError
  = UnboundName Token
  deriving (Show)

-- |Throw an |UnboundName| error using an arbitrary token.
unboundName :: (MonadError ScopeError m, IsToken name) => name -> m a
unboundName n = throwError $ UnboundName (toToken n)


-- * Scope checking contexts.

-- |Type of scope checking contexts.
type Ctx = HashMap Sort [Symbol]

-- |Create a context with a single symbol of the given sort.
singletonCtx :: Sort -> Symbol -> Ctx
singletonCtx sort symbol = Map.singleton sort [symbol]

-- |Find the index for a given name.
lookupIndex ::
  (MonadError ScopeError m, MonadReader Ctx m, IsToken name, KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K name sort -> m (DeBruijn sort)

lookupIndex (K n :: K name sort) = do
  ctx <- ask
  let symbols = Map.findWithDefault [] (sort @sort) ctx
  let maybeIndex = elemIndex (tkSym n) symbols
  let indexOrError = maybe (unboundName n) return maybeIndex
  fromIndex <$> indexOrError



-- * Scope checking.

-- |
checkScope ::
  (IsToken name, KnownSort sort) =>
  Tree (K name) builtin ann sort ->
  Except ScopeError (Tree DeBruijn builtin ann sort)
checkScope tree = DF.toReader (foldTree (DF . checkScopeF) tree) mempty

type SCOPE builtin ann sort = DATAFLOW Ctx (Except ScopeError) (Tree DeBruijn builtin ann) sort
type Scope builtin ann      = DataFlow Ctx (Except ScopeError) (Tree DeBruijn builtin ann)

-- |
checkScopeF ::
  forall name builtin ann sort.
  (IsToken name, KnownSort sort) =>
  TreeF (K name) builtin ann sort (Scope builtin ann) ->
  SCOPE builtin ann sort

checkScopeF = case sortSing @sort of

  -- Kinds
  --
  -- Kinds are always well-scoped,
  -- so the result for scope checking kinds is pure,
  -- and we simply reconstruct the type.
  --
  SKIND -> \case
    KAppF  ann k1 k2 -> KApp ann <$> unDF k1 <*> unDF k2
    KConF  ann op    -> return $ KCon ann op
    KMetaF ann i     -> return $ KMeta ann i

  -- Types
  --
  -- For binders, we add the name to the context as we recurse.
  -- For variables, we get the DeBruijn index as the position in the appropriate context.
  -- Otherwise, we simply recurse.
  --
  STYPE -> \case
    TForallF  ann n t   -> bindLocal n $ \n' -> TForall ann n' <$> unDF t
    TAppF     ann t1 t2 -> TApp ann <$> unDF t1 <*> unDF t2
    TVarF     ann n     -> TVar ann <$> lookupIndex n
    TConF     ann op    -> return $ TCon ann op
    TLitDimF  ann d     -> return $ TLitDim ann d
    TLitListF ann ts    -> TLitList ann <$> traverse unDF ts
    TMetaF    ann i     -> return $ TMeta ann i

  -- Type arguments
  --
  -- Return the argument as-is paired with its underlying symbol.
  --
  STARG -> \case
    TArgF ann n -> do let s = tkSym n
                      tell (singletonCtx TYPE s)
                      return $ TArg ann (fromSymbol s)

  -- Expressions
  --
  -- For binders, we add the name to the context as we recurse.
  -- For variables, we get the DeBruijn index as the position in the appropriate context.
  -- Otherwise, we simply recurse.
  --
  SEXPR -> \case
    EAnnF     ann e t     -> EAnn ann <$> unDF e <*> unDF t
    ELetF     ann n e1 e2 -> bindLocal n $ \n' -> ELet ann n' <$> unDF e1 <*> unDF e2
    ELamF     ann n e     -> bindLocal n $ \n' -> ELam ann n' <$> unDF e
    EAppF     ann e1 e2   -> EApp ann <$> unDF e1 <*> unDF e2
    EVarF     ann n       -> EVar ann <$> lookupIndex n
    ETyAppF   ann e t     -> ETyApp ann <$> unDF e <*> unDF t
    ETyLamF   ann n e     -> bindLocal n $ \n' -> ETyLam ann n' <$> unDF e
    EConF     ann op      -> return $ ECon ann op
    ELitIntF  ann z       -> return $ ELitInt ann z
    ELitRealF ann r       -> return $ ELitReal ann r
    ELitSeqF  ann es      -> ELitSeq ann <$> traverse unDF es

  -- Expression arguments
  --
  -- Return the argument as-is paired with its underlying symbol.
  --
  SEARG -> \case
    EArgF ann n -> do let s = tkSym n
                      tell (singletonCtx EXPR s)
                      return $ EArg ann (fromSymbol s)

  -- Declarations
  SDECL -> \case
    DeclNetwF ann n t    -> do n' <- bind n
                               t' <- passCtx t
                               return $ DeclNetw ann n' t'
    DeclDataF ann n t    -> do n' <- bind n
                               t' <- passCtx t
                               return $ DeclData ann n' t'
    DefTypeF  ann n ns t -> do n' <- bind n
                               bindAllLocal ns $ \ns' ->
                                 DefType ann n' ns' <$> unDF t
    DefFunF   ann n t e  -> do t' <- passCtx t
                               n' <- bind n
                               e' <- passCtx e
                               return $ DefFun ann n' t' e'

  -- Programs
  SPROG -> \case
    MainF ann ds -> stateToReader $ Main ann <$> traverse unDF ds

-- |Pass the context from the state monad to the reader monad.
passCtx ::
  (KnownSort sort, sort `In` ['TYPE, 'EXPR, 'PROG]) =>
  Scope builtin ann sort ->
  StateT Ctx (Except ScopeError) (Tree DeBruijn builtin ann sort)
passCtx df = readerToState (asReader df)

-- |Bind the given name in the state monad.
bind ::
  (KnownSort sort, sort `In` ['TARG, 'EARG]) =>
  Scope builtin ann sort ->
  StateT Ctx (Except ScopeError) (Tree DeBruijn builtin ann sort)
bind df = writerToState (asWriter df)

-- |Bind the given name /locally/ in the reader monad.
bindLocal ::
  (KnownSort sort, sort `In` ['TARG, 'EARG]) =>
  Scope builtin ann sort ->
  (Tree DeBruijn builtin ann sort -> ReaderT Ctx (Except ScopeError) a) -> ReaderT Ctx (Except ScopeError) a
bindLocal df k = writerToReader (asWriter df) k

-- |Bind a series of names /locally/ in a reader monad, then embeds the resulting value in a state monad.
bindAllLocal ::
  (KnownSort sort, sort `In` ['TARG, 'EARG]) =>
  [Scope builtin ann sort] ->
  ([Tree DeBruijn builtin ann sort] -> ReaderT Ctx (Except ScopeError) a) -> StateT Ctx (Except ScopeError) a
bindAllLocal = (readerToState .) . bindAllLocal'
  where
    bindAllLocal' ::
      (KnownSort sort, sort `In` ['TARG, 'EARG]) =>
      [Scope builtin ann sort] ->
      ([Tree DeBruijn builtin ann sort] -> ReaderT Ctx (Except ScopeError) a) -> ReaderT Ctx (Except ScopeError) a
    bindAllLocal' []       k = k []
    bindAllLocal' (df:dfs) k = bindLocal df (\n -> bindAllLocal' dfs (\ns -> k (n:ns)))

-- -}
-- -}
-- -}
-- -}
-- -}
