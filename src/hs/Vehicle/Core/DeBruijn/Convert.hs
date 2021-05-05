{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vehicle.Core.DeBruijn.Convert
  ( DBConvert(convert)
  , DeBruijnConversionError(..)
  , MonadDBConvert
  , runConvert
  ) where


import Vehicle.Core.Type
import Vehicle.Core.DeBruijn.Core (SortedDeBruijn(..), DeBruijnIndex(..))
import qualified Data.List as List
import Vehicle.Core.Abs (Name(..))
import Control.Monad.Except (MonadError, Except, runExcept)
import Control.Monad.Error.Class (throwError)
import Data.Text (Text)

-- * Conversion from names to de Bruijn indices

-- |Errors thrown during conversion from names to de Bruijn indices
data DeBruijnConversionError = FreeVariableError Text
  deriving (Show)

-- |Monad stack used during conversion from names to de Bruijn indices.
type MonadDBConvert m = MonadError DeBruijnConversionError m

-- |Context for de Bruijn conversion.
-- A list of the bound variable names encountered so far ordered from most to least recent.
type Context = [Text]

-- |Run a function in 'MonadDBConvert'.
runConvert :: Except DeBruijnConversionError a -> Either DeBruijnConversionError a
runConvert = runExcept

-- |Class for the various conversion functions.
class DBConvert f t where
  convert :: MonadDBConvert m => Context -> f -> m t

instance DBConvert (Kind (K Name) builtin ann) (Kind SortedDeBruijn builtin ann) where
  convert _ (KCon ann builtin) = return $ KCon ann builtin
  convert _ (KMeta ann var) = return $ KMeta ann var

  convert ctxt (KApp ann fn arg) = KApp ann <$> convert ctxt fn <*> convert ctxt arg

instance DBConvert (Type (K Name) builtin ann) (Type SortedDeBruijn builtin ann) where
  convert _ (TCon ann builtin) = return $ TCon ann builtin
  convert _ (TLitDim ann dim) = return $ TLitDim ann dim
  convert _ (TMeta ann var) = return $ TMeta ann var

  convert ctxt (TApp ann fn arg) = TApp ann <$> convert ctxt fn <*> convert ctxt arg
  convert ctxt (TLitList ann typs) = TLitList ann <$> traverse (convert ctxt) typs

  convert ctxt (TForall ann arg body) =
    let (name, cArg) = convertTArg arg in do
      cBody <- convert (name : ctxt) body
      return $ TForall ann cArg cBody

  convert ctxt (TVar ann (K name)) = do
    index <- convertName name ctxt
    return $ TVar ann (SortedDeBruijn index)

instance DBConvert (Expr (K Name) builtin ann) (Expr SortedDeBruijn builtin ann) where
  convert _ (ELitInt ann val) = return $ ELitInt ann val
  convert _ (ELitReal ann val) = return $ ELitReal ann val
  convert _ (ECon ann builtin) = return $ ECon ann builtin

  convert ctxt (ELitSeq ann exprs) = ELitSeq ann <$> traverse (convert ctxt) exprs
  convert ctxt (EAnn ann expr typ) = EAnn ann <$> convert ctxt expr<*> convert ctxt typ
  convert ctxt (ETyApp ann expr typ) = ETyApp ann <$> convert ctxt expr <*> convert ctxt typ
  convert ctxt (EApp ann expr1 expr2) = EApp ann <$> convert ctxt expr1 <*> convert ctxt expr2

  convert ctxt (EVar ann (K name)) = do
    index <- convertName name ctxt
    return $ EVar ann (SortedDeBruijn index)

  convert ctxt (ELet ann arg expr1 expr2) =
    let (varName, cArg) = convertEArg arg in do
      cExp1 <- convert ctxt expr1
      cExp2 <- convert (varName : ctxt) expr2
      return $ ELet ann cArg cExp1 cExp2

  convert ctxt (ELam ann arg expr) =
    let (varName, cArg) = convertEArg arg in do
      cExpr <- convert (varName : ctxt) expr
      return $ ELam ann cArg cExpr

  convert ctxt (ETyLam ann arg expr) =
    let (varName, cArg) = convertTArg arg in do
      cExpr <- convert (varName : ctxt) expr
      return $ ETyLam ann cArg cExpr

instance DBConvert (Decl (K Name) builtin ann) (Decl SortedDeBruijn builtin ann) where
  convert ctxt (DeclNetw ann arg typ) =
    let (varName, cArg) = convertEArg arg in do
      cTyp <- convert (varName : ctxt) typ
      return $ DeclNetw ann cArg cTyp

  convert ctxt (DeclData ann arg typ) =
    let (varName, cArg) = convertEArg arg in do
      cTyp <- convert (varName : ctxt) typ
      return $ DeclData ann cArg cTyp

  convert ctxt (DefType ann arg args typ) =
    let (varName, cArg) = convertTArg arg in do
      (varNames , cArgs) <- convertTArgs (varName : ctxt) args
      cTyp <- convert (reverse varNames ++ (varName : ctxt)) typ
      return $ DefType ann cArg cArgs cTyp

  convert ctxt (DefFun ann arg typ expr) = do
    let (varName, cArg) = convertEArg arg in do
      cTyp <- convert (varName : ctxt) typ
      cExpr <- convert (varName : ctxt) expr
      return $ DefFun ann cArg cTyp cExpr

instance DBConvert (Prog (K Name) builtin ann) (Prog SortedDeBruijn builtin ann) where
  convert ctxt (Main ann decls)= Main ann <$> convertDecls ctxt decls

convertEArg :: EArg (K Name) builtin ann -> (Text , EArg SortedDeBruijn builtin ann)
convertEArg (EArg ann (K name)) = (nameText name , EArg ann (SortedDeBruijn name))

convertTArg :: TArg (K Name) builtin ann -> (Text , TArg SortedDeBruijn builtin ann)
convertTArg (TArg ann (K name)) = (nameText name , TArg ann (SortedDeBruijn name))

convertTArgs :: MonadDBConvert m => Context -> [TArg (K Name) builtin ann] -> m ([Text], [TArg SortedDeBruijn builtin ann])
convertTArgs _ [] = return ([], [])
convertTArgs ctxt (tArg : tArgs) =
    let (varName, cArg) = convertTArg tArg in do
      (varNames, cArgs) <- convertTArgs (varName : ctxt) tArgs
      return (varName : varNames,  cArg : cArgs)

convertDecls :: MonadDBConvert m => Context -> [Decl (K Name) builtin ann] -> m [Decl SortedDeBruijn builtin ann]
convertDecls _ [] = return []
convertDecls ctxt (decl : decls) = do
  cDecl <- convert ctxt decl
  cDecls <- convertDecls (declText decl : ctxt) decls
  return (cDecl : cDecls)

convertName :: MonadDBConvert m => Name -> Context -> m DeBruijnIndex
convertName (Name (pos , text)) ctxt = case List.elemIndex text ctxt of
  Nothing -> throwError $ FreeVariableError text
  Just index -> return $ DeBruijnIndex (pos , index)

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
