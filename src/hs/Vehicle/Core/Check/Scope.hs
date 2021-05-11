{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Vehicle.Core.Check.Scope where

import           Control.Monad.Except (MonadError, Except, runExcept, liftEither)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Vehicle.Core.Type
import           Vehicle.Prelude


-- |Type of scope checking contexts.
data Ctx = Ctx { tEnv :: [Symbol], eEnv :: [Symbol] }

-- |The empty scope checking context.
emptyCtx :: Ctx
emptyCtx = Ctx { tEnv = [], eEnv = [] }

-- |Evaluates the |Reader| portions of the monad stack used in scope checking.
runScope :: ReaderT Ctx (Except ScopeError) a -> Except ScopeError a
runScope m = runReaderT m emptyCtx

-- |Type of errors thrown by scope checking.
data ScopeError
  = UnboundName Token
  | UnexpectedName Sort Token
  deriving (Show)

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

-- |
checkScope ::
  (IsToken name) =>
  Tree (K name) builtin ann 'PROG ->
  Except ScopeError (Tree DeBruijn builtin ann 'PROG)
checkScope tree = runScope (unR (foldTree checkScopeF tree))

-- private
type family ResultType (builtin :: Sort -> *) (ann :: Sort -> *) (sort :: Sort) :: * where
  ResultType builtin ann 'TARG = (Tree DeBruijn builtin ann 'TARG, Symbol)
  ResultType builtin ann 'EARG = (Tree DeBruijn builtin ann 'EARG, Symbol)
  ResultType builtin ann  sort = ReaderT Ctx (Except ScopeError) (Tree DeBruijn builtin ann sort)

-- private
newtype Result (builtin :: Sort -> *) (ann :: Sort -> *) (sort :: Sort)
  = R { unR :: ResultType builtin ann sort }

-- |
checkScopeF ::
  (IsToken name, KnownSort sort) =>
  TreeF (K name) builtin ann sort (Result builtin ann) ->
  Result builtin ann sort
checkScopeF (tree :: TreeF name builtin ann sort tree) = case sortSing :: SSort sort of

  -- Kinds
  SKIND -> case tree of
    KAppF  _ann k1 k2 -> undefined
    KConF  _ann op    -> undefined
    KMetaF _ann i     -> undefined

  -- Types
  STYPE -> case tree of
    TForallF  _ann n t   -> undefined
    TAppF     _ann t1 t2 -> undefined
    TVarF     _ann n     -> undefined
    TConF     _ann op    -> undefined
    TLitDimF  _ann d     -> undefined
    TLitListF _ann ts    -> undefined
    TMetaF    _ann i     -> undefined

  -- Type arguments
  STARG -> case tree of
    TArgF ann n -> let s = tkSym n in R (TArg ann (TSym s), s)

  -- Expressions
  SEXPR -> case tree of
    EAnnF     _ann e t     -> undefined
    ELetF     _ann n e1 e2 -> undefined
    ELamF     _ann n e     -> undefined
    EAppF     _ann e1 e2   -> undefined
    EVarF     _ann n       -> undefined
    ETyAppF   _ann e t     -> undefined
    ETyLamF   _ann n e     -> undefined
    EConF     _ann op      -> undefined
    ELitIntF  _ann z       -> undefined
    ELitRealF _ann r       -> undefined
    ELitSeqF  _ann es      -> undefined

  -- Expression arguments
  SEARG -> case tree of
    EArgF ann n -> let s = tkSym n in R (EArg ann (ESym s), s)

  -- Declarations
  SDECL -> case tree of
    DeclNetwF _ann n t    -> undefined
    DeclDataF _ann n t    -> undefined
    DefTypeF  _ann n ns t -> undefined
    DefFunF   _ann n t e  -> undefined

  -- Programs
  SPROG -> case tree of
    MainF _ann ds -> undefined

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

nameSymbol :: Name -> Symbol
nameSymbol (Name (_ , name)) = name

eArgName :: EArg (K Name) builtin ann -> Symbol
eArgName (EArg _ (K name))= nameSymbol name

tArgName :: TArg (K Name) builtin ann -> Symbol
tArgName (TArg _ (K name))= nameSymbol name

declName :: Decl (K Name) builtin ann -> Symbol
declName (DeclNetw _ arg _) = eArgName arg
declName (DeclData _ arg _) = eArgName arg
declName (DefFun _ arg _ _) = eArgName arg
declName (DefType _ arg _ _) = tArgName arg

-- -}
-- -}
-- -}
-- -}
-- -}
