{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Vehicle.Core.Compile.Scope
  ( checkScope
  , ScopeError(..)
  ) where

import           Control.Monad.Except (MonadError(..), Except)
import           Control.Monad.Reader (MonadReader(..), ReaderT)
import           Control.Monad.Writer (MonadWriter(..))
import           Data.Sequence (Seq, (!?))
import qualified Data.Sequence as Seq
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
-- type Ctx = HashMap Sort [Symbol]
data Ctx = Ctx { typeSymbols :: Seq Symbol, exprSymbols :: Seq Symbol }

instance Semigroup Ctx where
  Ctx typeSymbols1 exprSymbols1 <> Ctx typeSymbols2 exprSymbols2 =
    Ctx (typeSymbols1 <> typeSymbols2) (exprSymbols1 <> exprSymbols2)

instance Monoid Ctx where
  mempty = Ctx mempty mempty

-- |Create a context with a single symbol of the given sort.
singletonCtx :: (sort `In` ['TYPE, 'EXPR]) => SSort sort -> Symbol -> Ctx
singletonCtx STYPE symbol = Ctx (Seq.singleton symbol) Seq.empty
singletonCtx SEXPR symbol = Ctx Seq.empty (Seq.singleton symbol)

-- |Get the sub-context for a given sort.
getSubCtxFor :: forall sort. (KnownSort sort, sort `In` ['TYPE, 'EXPR]) => Ctx -> Seq Symbol
getSubCtxFor Ctx{..} = case sortSing @sort of { STYPE -> typeSymbols; SEXPR -> exprSymbols }

-- |Find the index for a given name of a given sort.
getIndex ::
  (IsToken name, KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K name sort -> ReaderT Ctx (Except ScopeError) (DeBruijn sort)

getIndex (K n :: K name sort) = do
  subctx <- getSubCtxFor @sort <$> ask
  let maybeIndex = Seq.elemIndexL (tkSymbol n) subctx
  let indexOrError = maybe (unboundName n) return maybeIndex
  fromIndex <$> indexOrError



-- * Scope checking.

-- |Check if a tree is well-scoped, replacing name tokens with deBruijn indices.
checkScope ::
  (IsToken name, KnownSort sort) =>
  Tree (K name) builtin ann sort ->
  Except ScopeError (Tree DeBruijn builtin ann sort)
checkScope tree = DF.toReader (foldTree (DF . checkScopeF) tree) mempty

type SCOPE builtin ann sort = DATAFLOW Ctx (Except ScopeError) (Tree DeBruijn builtin ann) sort
type Scope builtin ann      = DataFlow Ctx (Except ScopeError) (Tree DeBruijn builtin ann)

-- |Check if a single layer is well-scoped in the appropriate data-flow context.
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
    TForallF     ann n t   -> bindLocal n $ \n' -> TForall ann n' <$> unDF t
    TAppF        ann t1 t2 -> TApp ann <$> unDF t1 <*> unDF t2
    TVarF        ann n     -> TVar ann <$> getIndex n
    TConF        ann op    -> return $ TCon ann op
    TLitDimF     ann d     -> return $ TLitDim ann d
    TLitDimListF ann ts    -> TLitDimList ann <$> traverse unDF ts
    TMetaF       ann i     -> return $ TMeta ann i

  -- Type arguments
  --
  -- Return the argument as-is paired with its underlying symbol.
  --
  STARG -> \case
    TArgF ann n -> do let s = tkSymbol n
                      tell (singletonCtx STYPE s)
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
    EVarF     ann n       -> EVar ann <$> getIndex n
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
    EArgF ann n -> do let s = tkSymbol n
                      tell (singletonCtx SEXPR s)
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

-- -}
-- -}
-- -}
-- -}
-- -}
