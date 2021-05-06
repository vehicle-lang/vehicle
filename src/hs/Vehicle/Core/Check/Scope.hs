{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vehicle.Core.DeBruijn.Scope
  ( Scope(checkScope)
  , ScopeError(..)
  , MonadScope
  , runScope
  ) where


import Data.Functor.Foldable (fold)
import Vehicle.Core.Type
import Vehicle.Core.DeBruijn.Core (SortedDeBruijn(..), Ix(..))
import qualified Data.List as List
import Vehicle.Core.Abs (Name(..))
import Control.Monad.Except (MonadError, Except, runExcept)
import Control.Monad.Error.Class (throwError)
import Data.Text (Text)

-- * Conversion from names to de Bruijn indices

-- |Errors thrown during conversion from names to de Bruijn indices
newtype ScopeError = UnboundName Text
  deriving (Show)

-- |Monad stack used during conversion from names to de Bruijn indices.
type MonadScope m = MonadError ScopeError m

-- |Context for de Bruijn conversion.
-- A list of the bound variable names encountered so far ordered from most to least recent.
type Context = [Text]

-- |Run a function in 'MonadScope'.
runScope :: Except ScopeError a -> Either ScopeError a
runScope = runExcept

-- |Class for the various conversion functions.
class Scope tree1 tree2 where
  checkScope :: MonadScope m => Context -> tree1 -> m tree2

instance Scope (Kind (K Name) builtin ann) (Kind SortedDeBruijn builtin ann) where
  checkScope _ctx = fold $ \case
    KConF  ann op    -> return $ KCon ann op
    KMetaF ann i     -> return $ KMeta ann i
    KAppF  ann k1 k2 -> KApp ann <$> k1 <*> k2

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

checkScopeEArg :: EArg (K Name) builtin ann -> (Text , EArg SortedDeBruijn builtin ann)
checkScopeEArg (EArg ann (K name)) = (nameText name , EArg ann (SortedDeBruijn name))

checkScopeTArg :: TArg (K Name) builtin ann -> (Text , TArg SortedDeBruijn builtin ann)
checkScopeTArg (TArg ann (K name)) = (nameText name , TArg ann (SortedDeBruijn name))

checkScopeTArgs :: MonadScope m => Context -> [TArg (K Name) builtin ann] -> m ([Text], [TArg SortedDeBruijn builtin ann])
checkScopeTArgs _ [] = return ([], [])
checkScopeTArgs ctxt (tArg : tArgs) =
    let (varName, cArg) = checkScopeTArg tArg in do
      (varNames, cArgs) <- checkScopeTArgs (varName : ctxt) tArgs
      return (varName : varNames,  cArg : cArgs)

checkScopeDecls :: MonadScope m => Context -> [Decl (K Name) builtin ann] -> m [Decl SortedDeBruijn builtin ann]
checkScopeDecls _ [] = return []
checkScopeDecls ctxt (decl : decls) = do
  cDecl <- checkScope ctxt decl
  cDecls <- checkScopeDecls (declText decl : ctxt) decls
  return (cDecl : cDecls)

checkScopeName :: MonadScope m => Name -> Context -> m Ix
checkScopeName (Name (pos , text)) ctxt = case List.elemIndex text ctxt of
  Nothing -> throwError $ UnboundName text
  Just index -> return $ Ix (pos , index)

-- * Helper functions for extracting the Text from binding sites

nameText :: Name -> Text
nameText (Name (_ , name)) = name

eArgText :: EArg (K Name) builtin ann -> Text
eArgText (EArg _ (K name))= nameText name

tArgText :: TArg (K Name) builtin ann -> Text
tArgText (TArg _ (K name))= nameText name

declText :: Decl (K Name) builtin ann -> Text
declText (DeclNetw _ arg _) = eArgText arg
declText (DeclData _ arg _) = eArgText arg
declText (DefFun _ arg _ _) = eArgText arg
declText (DefType _ arg _ _) = tArgText arg
