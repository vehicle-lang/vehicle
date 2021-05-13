{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Vehicle.Core.Compile.Scope where

import           Control.Monad.Except (MonadError(..), Except, runExcept, liftEither)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Trans (MonadTrans(..))
import           Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT)
import           Control.Monad.State (MonadState(..), StateT, modify, evalStateT)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List (elemIndex)
import           Data.Maybe (fromMaybe)
import           Vehicle.Core.AST
import           Vehicle.Prelude


-- |
checkScope ::
  (IsToken name, KnownSort sort) =>
  Tree (K name) builtin ann sort ->
  Except ScopeError (Tree DeBruijn builtin ann sort)
checkScope tree = fromResult (foldTree (R . checkScopeF) tree)

-- private
type family RESULT (builtin :: Sort -> *) (ann :: Sort -> *) (sort :: Sort) :: * where
  RESULT builtin ann 'KIND = Kind DeBruijn builtin ann
  RESULT builtin ann 'TYPE = ReaderT Ctx (Except ScopeError) (Type DeBruijn builtin ann)
  RESULT builtin ann 'EXPR = ReaderT Ctx (Except ScopeError) (Expr DeBruijn builtin ann)
  RESULT builtin ann 'DECL = StateT  Ctx (Except ScopeError) (Decl DeBruijn builtin ann)
  RESULT builtin ann 'PROG = ReaderT Ctx (Except ScopeError) (Prog DeBruijn builtin ann)
  RESULT builtin ann 'TARG = (TArg DeBruijn builtin ann, Symbol)
  RESULT builtin ann 'EARG = (EArg DeBruijn builtin ann, Symbol)

-- private
newtype Result (builtin :: Sort -> *) (ann :: Sort -> *) (sort :: Sort)
  = R { unR :: RESULT builtin ann sort }

-- private
fromResult ::
  forall builtin ann sort.
  (KnownSort sort) =>
  Result builtin ann sort -> Except ScopeError (Tree DeBruijn builtin ann sort)
fromResult = case sortSing @sort of
  SKIND -> return . unR
  STYPE -> flip runReaderT emptyCtx . unR
  SEXPR -> flip runReaderT emptyCtx . unR
  SDECL -> flip evalStateT emptyCtx . unR
  SPROG -> flip runReaderT emptyCtx . unR
  STARG -> return . fst . unR
  SEARG -> return . fst . unR

-- |
checkScopeF ::
  forall name builtin ann sort.
  (IsToken name, KnownSort sort) =>
  TreeF (K name) builtin ann sort (Result builtin ann) ->
  RESULT builtin ann sort

checkScopeF = case sortSing @sort of

  -- Kinds
  --
  -- Kinds are always well-scoped,
  -- so the result for scope checking kinds is pure,
  -- and we simply reconstruct the type.
  --
  SKIND -> \case
    KAppF  ann k1 k2 -> KApp  ann (unR k1) (unR k2)
    KConF  ann op    -> KCon  ann op
    KMetaF ann i     -> KMeta ann i

  -- Types
  --
  -- For binders, we add the name to the context as we recurse.
  -- For variables, we get the DeBruijn index as the position in the appropriate context.
  -- Otherwise, we simply recurse.
  --
  STYPE -> \case
    TForallF  ann n t   -> bindLocal n $ (\n' -> TForall ann n' <$> unR t)
    TAppF     ann t1 t2 -> TApp ann <$> unR t1 <*> unR t2
    TVarF     ann n     -> TVar ann <$> getIndex n
    TConF     ann op    -> return $ TCon ann op
    TLitDimF  ann d     -> return $ TLitDim ann d
    TLitListF ann ts    -> TLitList ann <$> traverse unR ts
    TMetaF    ann i     -> return $ TMeta ann i

  -- Type arguments
  --
  -- Return the argument as-is paired with its underlying symbol.
  --
  STARG -> \case
    TArgF ann n -> let s = tkSym n in (TArg ann (fromSymbol s), s)

  -- Expressions
  --
  -- For binders, we add the name to the context as we recurse.
  -- For variables, we get the DeBruijn index as the position in the appropriate context.
  -- Otherwise, we simply recurse.
  --
  SEXPR -> \case
    EAnnF     ann e t     -> EAnn ann <$> unR e <*> unR t
    ELetF     ann n e1 e2 -> bindLocal n $ (\n' -> ELet ann n' <$> unR e1 <*> unR e2)
    ELamF     ann n e     -> bindLocal n $ (\n' -> ELam ann n' <$> unR e)
    EAppF     ann e1 e2   -> EApp ann <$> unR e1 <*> unR e2
    EVarF     ann n       -> EVar ann <$> getIndex n
    ETyAppF   ann e t     -> ETyApp ann <$> unR e <*> unR t
    ETyLamF   ann n e     -> bindLocal n $ (\n' -> ETyLam ann n' <$> unR e)
    EConF     ann op      -> return $ ECon ann op
    ELitIntF  ann z       -> return $ ELitInt ann z
    ELitRealF ann r       -> return $ ELitReal ann r
    ELitSeqF  ann es      -> ELitSeq ann <$> traverse unR es

  -- Expression arguments
  --
  -- Return the argument as-is paired with its underlying symbol.
  --
  SEARG -> \case
    EArgF ann n -> let s = tkSym n in (EArg ann (fromSymbol s), s)

  -- Declarations
  SDECL -> \case
    DeclNetwF ann n t    -> DeclNetw ann <$> bind n <*> readerToState (unR t)
    DeclDataF ann n t    -> DeclData ann <$> bind n <*> readerToState (unR t)
    DefTypeF  ann n ns t -> do n' <- bind n
                               readerToState . bindAllLocal ns $ \ns' ->
                                 DefType ann n' ns' <$> unR t
    DefFunF   ann n t e  -> DefFun ann <$> bind n <*>
                              readerToState (unR t)  <*> readerToState (unR e)

  -- Programs
  SPROG -> \case
    MainF ann ds -> undefined


-- * Contexts

-- |Type of scope checking contexts.
type Ctx = HashMap Sort [Symbol]

-- |The empty scope checking context.
emptyCtx :: Ctx
emptyCtx = Map.empty

-- |Extend a context for the given sort with the given symbol.
extWith :: Sort -> Symbol -> Ctx -> Ctx
extWith sort s = Map.alter (\mss -> Just (s:fromMaybe [] mss)) sort

-- |Extend the context in a reader monad with the given symbols.
bindAllLocal ::
  (MonadReader Ctx m, KnownSort sort, sort `In` ['TARG, 'EARG]) =>
  [Result builtin ann sort] -> ([Tree DeBruijn builtin ann sort] -> m a) -> m a

bindAllLocal []     k = k []
bindAllLocal (r:rs) k = bindLocal r (\r' -> bindAllLocal rs (\rs' -> k (r':rs')))

-- |Extend the context in a reader monad with the given symbol.
bindLocal ::
  (MonadReader Ctx m, KnownSort sort, sort `In` ['TARG, 'EARG]) =>
  Result builtin ann sort -> (Tree DeBruijn builtin ann sort -> m a) -> m a

bindLocal (r :: Result builtin ann sort) k = case sortSing @sort of
  STARG -> do let (n, s) = unR r
              local (TYPE `extWith` s) (k n)
  SEARG -> do let (n, s) = unR r
              local (EXPR `extWith` s) (k n)

-- |Extend the context in a state monad with the given symbol of the given sort.
bind ::
  (MonadState Ctx m, KnownSort sort, sort `In` ['TARG, 'EARG]) =>
  Result builtin ann sort -> m (Tree DeBruijn builtin ann sort)

bind (r :: Result builtin ann sort) = case sortSing @sort of
  STARG -> do let (n, s) = unR r
              modify (TYPE `extWith` s)
              return n
  SEARG -> do let (n, s) = unR r
              modify (EXPR `extWith` s)
              return n

-- |Get the index
getIndex ::
  (MonadError ScopeError m, MonadReader Ctx m, IsToken name, KnownSort sort, sort `In` ['TYPE, 'EXPR]) =>
  K name sort -> m (DeBruijn sort)

getIndex (K n :: K name sort) = fromIndex <$> index
  where
    index = do ss <- Map.findWithDefault [] (sort @sort) <$> ask
               maybe (unboundName n) return (elemIndex (tkSym n) ss)


-- * Scope errors

-- |Type of errors thrown by scope checking.
data ScopeError
  = UnboundName Token
  | UnexpectedName Sort Token
  deriving (Show)

-- |Throw an |UnboundName| error.
unboundName ::
  (MonadError ScopeError m, IsToken name) => name -> m a
unboundName n =
  throwError $ UnboundName (toToken n)

-- |Throw an |UnexpectedName| error.
--
-- NOTE: No |UnexpectedName| error should ever be thrown. No names are used in,
--       e.g., kinds, but the parser does not guarantee this constraint.
--
unexpectedName ::
  (MonadError ScopeError m, IsToken name, KnownSort sort) =>
  K name sort -> m (DeBruijn sort)
unexpectedName (K n :: K name sort) =
  throwError $ UnexpectedName (sort @sort) (toToken n)



-- * Helper functions

-- |Convert a reader monad to a state monad.
readerToState :: Monad m => ReaderT s m a -> StateT s m a
readerToState m = do x <- get; lift (runReaderT m x)

-- |Convert a state monad to a reader monad.
stateToReader :: Monad m => StateT s m a -> ReaderT s m a
stateToReader m = do x <- ask; lift (evalStateT m x)

-- -}
-- -}
-- -}
-- -}
-- -}
