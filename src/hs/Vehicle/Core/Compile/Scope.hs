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
    DeclNetwF ann n t    -> do bind n $ \n' -> DeclNetw ann n' <$> readerToState (unR t)
    DeclDataF ann n t    -> do bind n $ \n' -> DeclData ann n' <$> readerToState (unR t)
    DefTypeF  ann n ns t -> undefined
    DefFunF   ann n t e  -> undefined

  -- Programs
  SPROG -> \case
    MainF ann ds -> undefined

readerToState :: Monad m => ReaderT s m a -> StateT s m a
readerToState m = do x <- get; lift (runReaderT m x)

stateToReader :: Monad m => StateT s m a -> ReaderT s m a
stateToReader m = do x <- ask; lift (evalStateT m x)

-- * Contexts

-- |Type of scope checking contexts.
type Ctx = HashMap Sort [Symbol]

-- |The empty scope checking context.
emptyCtx :: Ctx
emptyCtx = Map.empty

-- |Extend a context for the given sort with the given symbol.
extWith :: Sort -> Symbol -> Ctx -> Ctx
extWith sort s = Map.alter (\mss -> Just (s:fromMaybe [] mss)) sort

-- |Extend the context in a reader monad with the given symbol of the given sort.
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

{-

-- * Conversion from names to de Bruijn indices

-- |Errors thrown during conversion from names to de Bruijn indices
newtype ScopeError = UnboundName Symbol
  deriving (Show)

-- |Monad stack used during conversion from names to de Bruijn indices.
type MonadScope m = MonadError ScopeError m

-- |Context for de Bruijn conversion.
-- A list of the bound variable names encountered so far ordered from most to least recent.
data Ctx = Ctx { kenv :: [Symbol], tenv :: [Symbol] }

-- |Run a function in 'MonadScope'.
runScope :: Except ScopeError a -> Either ScopeError a
runScope = runExcept

-- |Class for the various conversion functions.
class Scope tree1 tree2 where
  checkScope :: MonadScope m => Context -> tree1 -> m tree2
{-
instance Scope (Kind (K Name) builtin ann) (Kind SortedDeBruijn builtin ann) where
  checkScope _ctx = fold $ \case
    KConF  ann op    -> return $ KCon ann op
    KMetaF ann i     -> return $ KMeta ann i
    KAppF  ann k1 k2 -> KApp ann <$> k1 <*> k2
-}
instance Scope (Type (K Name) builtin ann) (Type SortedDeBruijn builtin ann) where
  checkScope _ (TCon ann builtin) = return $ TCon ann builtin
  checkScope _ (TLitDim ann dim) = return $ TLitDim ann dim
  checkScope _ (TMeta ann var) = return $ TMeta ann var

  checkScope ctxt (TApp ann fn arg) = TApp ann <$> checkScope ctxt fn <*> checkScope ctxt arg
  checkScope ctxt (TLitList ann typs) = TLitList ann <$> traverse (checkScope ctxt) typs

  checkScope ctxt (TForall ann arg body) =
    let (name, cArg) = checkScopeTArg arg in do
      cBody <- checkScope (name : ctxt) body
      return $ TForall ann cArg cBody

  checkScope ctxt (TVar ann (K name)) = do
    index <- checkScopeName name ctxt
    return $ TVar ann (SortedDeBruijn index)

instance Scope (Expr (K Name) builtin ann) (Expr SortedDeBruijn builtin ann) where
  checkScope _ (ELitInt ann val) = return $ ELitInt ann val
  checkScope _ (ELitReal ann val) = return $ ELitReal ann val
  checkScope _ (ECon ann builtin) = return $ ECon ann builtin

  checkScope ctxt (ELitSeq ann exprs) = ELitSeq ann <$> traverse (checkScope ctxt) exprs
  checkScope ctxt (EAnn ann expr typ) = EAnn ann <$> checkScope ctxt expr<*> checkScope ctxt typ
  checkScope ctxt (ETyApp ann expr typ) = ETyApp ann <$> checkScope ctxt expr <*> checkScope ctxt typ
  checkScope ctxt (EApp ann expr1 expr2) = EApp ann <$> checkScope ctxt expr1 <*> checkScope ctxt expr2

  checkScope ctxt (EVar ann (K name)) = do
    index <- checkScopeName name ctxt
    return $ EVar ann (SortedDeBruijn index)

  checkScope ctxt (ELet ann arg expr1 expr2) =
    let (varName, cArg) = checkScopeEArg arg in do
      cExp1 <- checkScope ctxt expr1
      cExp2 <- checkScope (varName : ctxt) expr2
      return $ ELet ann cArg cExp1 cExp2

  checkScope ctxt (ELam ann arg expr) =
    let (varName, cArg) = checkScopeEArg arg in do
      cExpr <- checkScope (varName : ctxt) expr
      return $ ELam ann cArg cExpr

  checkScope ctxt (ETyLam ann arg expr) =
    let (varName, cArg) = checkScopeTArg arg in do
      cExpr <- checkScope (varName : ctxt) expr
      return $ ETyLam ann cArg cExpr

instance Scope (Decl (K Name) builtin ann) (Decl SortedDeBruijn builtin ann) where
  checkScope ctxt (DeclNetw ann arg typ) =
    let (varName, cArg) = checkScopeEArg arg in do
      cTyp <- checkScope (varName : ctxt) typ
      return $ DeclNetw ann cArg cTyp

  checkScope ctxt (DeclData ann arg typ) =
    let (varName, cArg) = checkScopeEArg arg in do
      cTyp <- checkScope (varName : ctxt) typ
      return $ DeclData ann cArg cTyp

  checkScope ctxt (DefType ann arg args typ) =
    let (varName, cArg) = checkScopeTArg arg in do
      (varNames , cArgs) <- checkScopeTArgs (varName : ctxt) args
      cTyp <- checkScope (reverse varNames ++ (varName : ctxt)) typ
      return $ DefType ann cArg cArgs cTyp

  checkScope ctxt (DefFun ann arg typ expr) = do
    let (varName, cArg) = checkScopeEArg arg in do
      cTyp <- checkScope (varName : ctxt) typ
      cExpr <- checkScope (varName : ctxt) expr
      return $ DefFun ann cArg cTyp cExpr

instance Scope (Prog (K Name) builtin ann) (Prog SortedDeBruijn builtin ann) where
  checkScope ctxt (Main ann decls)= Main ann <$> checkScopeDecls ctxt decls

checkScopeEArg :: EArg (K Name) builtin ann -> (Symbol , EArg SortedDeBruijn builtin ann)
checkScopeEArg (EArg ann (K name)) = (nameSymbol name , EArg ann (SortedDeBruijn name))

checkScopeTArg :: TArg (K Name) builtin ann -> (Symbol , TArg SortedDeBruijn builtin ann)
checkScopeTArg (TArg ann (K name)) = (nameSymbol name , TArg ann (SortedDeBruijn name))

checkScopeTArgs :: MonadScope m => Context -> [TArg (K Name) builtin ann] -> m ([Symbol], [TArg SortedDeBruijn builtin ann])
checkScopeTArgs _ [] = return ([], [])
checkScopeTArgs ctxt (tArg : tArgs) =
    let (varName, cArg) = checkScopeTArg tArg in do
      (varNames, cArgs) <- checkScopeTArgs (varName : ctxt) tArgs
      return (varName : varNames,  cArg : cArgs)

checkScopeDecls :: MonadScope m => Context -> [Decl (K Name) builtin ann] -> m [Decl SortedDeBruijn builtin ann]
checkScopeDecls _ [] = return []
checkScopeDecls ctxt (decl : decls) = do
  cDecl <- checkScope ctxt decl
  cDecls <- checkScopeDecls (declName decl : ctxt) decls
  return (cDecl : cDecls)

checkScopeName :: MonadScope m => Name -> Context -> m Ix
checkScopeName (Name (pos , text)) ctxt = case List.elemIndex text ctxt of
  Nothing -> throwError $ UnboundName text
  Just index -> return $ Ix (pos , index)

-- * Helper functions for extracting the Symbol from binding sites


-- -}
-- -}
-- -}
-- -}
-- -}
