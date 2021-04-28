{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Vehicle.Core.Normalise
  ( Norm
  , NormError (..)
  , MonadNorm
  , runNorm
  ) where

import Data.Range (Range)
import Data.Text (Text)
import Vehicle.Prelude (Position)
import Vehicle.Core.Type ( Tree (..), Expr, Type, Decl, Prog, K(..), TArg, Sort(EXPR))
import Vehicle.Core.DeBruijn.Core (SortedDeBruijn(..), DeBruijnIndex(..))
import Vehicle.Core.DeBruijn.Substitution as DeBruijn
import Control.Monad.Except (MonadError, Except, runExcept)
import Vehicle.Core.Abs (ExprName, Builtin(..))
import Control.Monad.Error.Class (throwError)

-- |Errors thrown during normalisation
data NormError = MissingDefFunType ExprName
    | MalformedLambdaError
    | NotImplementedError

-- |Constraint for the monad stack used by the normaliser.
type MonadNorm m = MonadError NormError m

-- |Run a function in 'MonadNorm'.
runNorm :: Except NormError a -> Either NormError a
runNorm = runExcept

-- |Some useful type synonyms
type NormExpr ann = Expr SortedDeBruijn (K Builtin) ann
type NormType ann = Type SortedDeBruijn (K Builtin) ann
type NormDecl ann = Decl SortedDeBruijn (K Builtin) ann
type NormProg ann = Prog SortedDeBruijn (K Builtin) ann

-- |Class for the various normalisation functions.
-- Invariant is that everything in the context is fully normalised
class Norm vf where
  norm :: MonadNorm m => vf -> m vf

instance Norm (NormExpr ann) where
  norm expr@(ELitInt _ _) = return expr
  norm expr@(ELitReal _ _) = return expr
  norm expr@(ECon _ _) = return expr
  norm expr@(EVar _ _) = return expr

  norm (ELitSeq ann exprs) = ELitSeq ann <$> traverse norm exprs
  norm (EAnn ann expr typ) = EAnn ann <$> norm expr <*> norm typ
  norm (ELam ann arg expr) = ELam ann arg <$> norm expr
  norm (ETyApp ann expr typ) = ETyApp ann <$> norm expr <*> norm typ
  norm (ETyLam ann targ expr) = ETyLam ann targ <$> norm expr

  norm (ELet _ _ letValue letBody) = do
    normalisedLetValue <- norm letValue
    let letBodyWithSubstitution = DeBruijn.subst 0 normalisedLetValue letBody
    norm letBodyWithSubstitution

  norm (EApp ann fn arg) = do
    normalisedArg <- norm arg
    normalisedFn <- norm fn
    normApp (EApp ann normalisedFn normalisedArg)

instance Norm (NormType ann) where
  norm expr@(TCon _ _) = return expr
  norm expr@(TLitDim _ _) = return expr
  norm expr@(TMeta _ _) = return expr
  norm expr@(TVar _ _) = return expr

  norm (TLitList ann typs) = TLitList ann <$> traverse norm typs
  norm (TForall ann arg body) = TForall ann arg <$> norm body

  norm (TApp _ (TForall _ _ forallBody) arg) = do
    normalisedArg <- norm arg
    let substForallBody = DeBruijn.subst 0 normalisedArg forallBody
    norm substForallBody
  norm TApp {} = throwError MalformedLambdaError

instance Norm (NormDecl ann) where
  norm (DeclNetw ann arg typ) = DeclNetw ann arg <$> norm typ
  norm (DeclData ann arg typ) = DeclData ann arg <$> norm typ
  norm (DefType ann arg args typ) = DefType ann arg args <$> norm typ
  norm (DefFun ann arg typ expr) = DefFun ann arg <$> norm typ <*> norm expr

instance Norm (NormProg ann) where
  norm (Main ann decls)= Main ann <$> traverse norm decls

pattern EOp0 op ann0 pos = ECon ann0 (K (Builtin (pos , op)))
pattern EOp1 op e1 ann0 ann1 pos = EApp ann1 (EOp0 op ann0 pos) e1
pattern EOp2 op e1 e2 ann0 ann1 ann2 pos = EApp ann2 (EOp1 op e1 ann0 ann1 pos) e2
pattern EOp3 op e1 e2 e3 ann0 ann1 ann2 ann3 pos  = EApp ann3 (EOp2 op e1 e2 ann0 ann1 ann2 pos) e3

pattern ETrue ann pos = EOp0 "true" ann pos
pattern EFalse ann pos = EOp0 "false" ann pos

mkBool :: Bool -> ann 'EXPR -> Position -> NormExpr ann
mkBool True ann pos = EOp0 "true" ann pos 
mkBool False ann pos = EOp0 "false" ann pos

normApp :: 
  (MonadNorm m) =>
  NormExpr ann -> 
  m (NormExpr ann)
-- Lambda expressions
normApp (EApp _ (ELam _ _ funcBody) arg) = norm (DeBruijn.subst 0 arg funcBody)
-- Boolean builtins
normApp (EOp2 "==" (ETrue _ _) e2 _ _ _ _) = return e2
normApp (EOp2 "==" (EFalse _ _) e2 ann0 ann1 _ pos) = normApp $ EOp1 "not" e2 ann0 ann1 pos
normApp (EOp1 "not" (ETrue _ _) ann0 _ pos) = return $ EFalse ann0 pos
normApp (EOp1 "not" (EFalse _ _) ann0 _ pos) = return $ ETrue ann0 pos
normApp (EOp2 "and" (ETrue _ _) e2 _ _ _ _) = return e2
normApp (EOp2 "and" (EFalse _ _) _ ann0 _ _ pos) = return $ EFalse ann0 pos
normApp (EOp2 "and" e1 (ETrue _ _) _ _ _ _) = return e1
normApp (EOp2 "and" _ (EFalse _ _) ann0 _ _ pos) = return $ EFalse ann0 pos
normApp (EOp2 "or" (ETrue _ _) _ ann0 _ _ pos) = return $ ETrue ann0 pos
normApp (EOp2 "or" (EFalse _ _) e2 _ _ _ _) = return e2
normApp (EOp2 "or" _ (ETrue _ _) ann0 _ _ pos) = return $ ETrue ann0 pos
normApp (EOp2 "or" e1 (EFalse _ _) _ _ _ _) = return e1
-- See https://github.com/wenkokke/vehicle/issues/2
normApp (EOp3 "if" (ETrue _ _) e2 _ _ _ _ _ _) = return e2
normApp (EOp3 "if" (EFalse _ _) _ e3 _ _ _ _ _) = return e3
-- Natural builtins
normApp (EOp2 "==" (ELitInt _ i) (ELitInt _ j) ann0 _ _ pos) = return $ mkBool (i == j) ann0 pos
normApp (EOp2 "<=" (ELitInt _ i) (ELitInt _ j) ann0 _ _ pos) = return $ mkBool (i <= j) ann0 pos
normApp (EOp2 "<" (ELitInt _ i) (ELitInt _ j) ann0 _ _ pos) = return $ mkBool (i < j) ann0 pos
-- TODO implement associativity rules?
normApp (EOp2 "+" (ELitInt _ i) (ELitInt _ j) ann0 _ _ _) = return $ ELitInt ann0 (i + j)
normApp (EOp2 "-" (ELitInt _ i) (ELitInt _ j) ann0 _ _ _) = return $ ELitInt ann0 (i - j)
normApp (EOp2 "*" (ELitInt _ i) (ELitInt _ j) ann0 _ _ _) = return $ ELitInt ann0 (i * j)
normApp (EOp2 "/" (ELitInt _ i) (ELitInt _ j) ann0 _ _ _) = return $ ELitInt ann0 (i / j)
-- Real builtins
normApp (EOp2 "==" (ELitReal _ x) (ELitReal _ y) ann0 _ _ pos) = return $ mkBool (x == y) ann0 pos
normApp (EOp2 "<=" (ELitReal _ x) (ELitReal _ y) ann0 _ _ pos) = return $ mkBool (x <= y) ann0 pos
normApp (EOp2 "<" (ELitReal _ x) (ELitReal _ y) ann0 _ _ pos) = return $ mkBool (x < y) ann0 pos
-- TODO implement associativity rules?
normApp (EOp2 "+" (ELitReal _ x) (ELitReal _ y) ann0 _ _ _) = return $ ELitReal ann0 (x + y)
normApp (EOp2 "-" (ELitReal _ x) (ELitReal _ y) ann0 _ _ _) = return $ ELitReal ann0 (x - y)
normApp (EOp2 "*" (ELitReal _ x) (ELitReal _ y) ann0 _ _ _) = return $ ELitReal ann0 (x * y)
normApp (EOp2 "/" (ELitReal _ x) (ELitReal _ y) ann0 _ _ _) = return $ ELitReal ann0 (x / y)
normApp (EOp1 "~" (ELitReal _ x) ann0 _ _) = return $ ELitReal ann0 (- x)
-- Tensor builtins
normApp (EOp2 "::" e (ELitSeq _ es) ann0 _ _ _) = return $ ELitSeq ann0 (e : es)
normApp (EOp2 "!" (ELitSeq _ es) (ELitInt _ i) _ _ _ _) = return $ es !! fromIntegral i
normApp (EOp2 "!" (EOp2 "::" e1 _ _ _ _ _) (ELitInt _ 0) _ _ _ _) = return e1
normApp (EOp2 "!" (EOp2 "::" _ es _ _ _ _) (ELitInt ann3 i) ann0 ann1 ann2 pos) = normApp (EOp2 "!" es (ELitInt ann3 (i - 1)) ann0 ann1 ann2 pos)
-- Quantifier builtins
--normApp (EOp2 "All" _ _ _ _ _ _) = throwError NotImplementedError
--normApp (EOp2 "Any" _ _ _ _ _ _) = throwError NotImplementedError
-- Fall-through case
normApp expr = return expr