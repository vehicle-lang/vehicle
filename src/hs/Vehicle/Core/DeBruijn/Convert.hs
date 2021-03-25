{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vehicle.Core.DeBruijn.Convert
  ( DBConvert
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
newtype DeBruijnConversionError = FreeVariableError Name

-- |Monad stack used during conversion from names to de Bruijn indices.
type MonadDBConvert m = MonadError DeBruijnConversionError m

-- |Run a function in 'MonadDBConvert'.
runConvert :: Except DeBruijnConversionError a -> Either DeBruijnConversionError a
runConvert = runExcept

-- |Class for the various conversion functions.
class DBConvert f t where
  convert :: MonadDBConvert m => [Text] -> f -> m t

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

convertEArg :: EArg (K Name) builtin ann -> (Text , EArg SortedDeBruijn builtin ann)
convertEArg (EArg ann (K name)) = (getText name , EArg ann (SortedDeBruijn name))

convertTArg :: TArg (K Name) builtin ann -> (Text , TArg SortedDeBruijn builtin ann)
convertTArg (TArg ann (K name)) = (getText name , TArg ann (SortedDeBruijn name))

convertTArgs :: MonadDBConvert m => [Text] -> [TArg (K Name) builtin ann] -> m ([Text], [TArg SortedDeBruijn builtin ann])
convertTArgs _ [] = return ([], [])
convertTArgs ctxt (tArg : tArgs) =
    let (varName, cArg) = convertTArg tArg in do
      (varNames, cArgs) <- convertTArgs (varName : ctxt) tArgs
      return (varName : varNames,  cArg : cArgs)

convertName :: MonadDBConvert m => Name -> [Text] -> m DeBruijnIndex
convertName name@(Name (pos , text)) ctxt = case List.elemIndex text ctxt of
  Nothing -> throwError $ FreeVariableError name
  Just index -> return $ DeBruijnIndex (pos , index)

getText :: Name -> Text 
getText (Name (_ , name)) = name