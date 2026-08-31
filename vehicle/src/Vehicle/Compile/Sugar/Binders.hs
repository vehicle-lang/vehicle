{-# LANGUAGE ViewPatterns #-}

module Vehicle.Compile.Sugar.Binders
  ( HasBasicBinders (..),
    HasBuiltinBinders (..),
    foldPiBinders,
    foldLamBinders,
    foldQuantifierBinders,
    foldForeachBinders,
    foldDeclBinders,
    foldLetBinders,
    LetBinder,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Vehicle.Data.AST
import Vehicle.Data.Builtin.Core

-- This module deals with all the unfolding and folding of syntactic
-- sugar in the external language. The unfolding is designed so that it should
-- be 100% reversible.

--------------------------------------------------------------------------------
-- Pi/Fun/Forall declarations

class HasBasicBinders expr where
  getPiBinder :: expr -> Maybe (GenericBinder expr, expr)
  getLamBinder :: expr -> Maybe (GenericBinder expr, expr)
  getLetBinder :: expr -> Maybe (expr, GenericBinder expr, expr)

class HasBuiltinBinders expr where
  getQuantifierBinder :: Quantifier -> expr -> Maybe (GenericBinder expr, expr)
  getForeachBinder :: expr -> Maybe (GenericBinder expr, expr)

foldPiBinders ::
  (Show expr, HasBasicBinders expr) =>
  GenericBinder expr ->
  expr ->
  ([GenericBinder expr], expr)
foldPiBinders = foldBinders getPiBinder

foldLamBinders ::
  (Show expr, HasBasicBinders expr) =>
  GenericBinder expr ->
  expr ->
  ([GenericBinder expr], expr)
foldLamBinders = foldBinders getLamBinder

foldForeachBinders ::
  (Show expr, HasBuiltinBinders expr) =>
  GenericBinder expr ->
  expr ->
  ([GenericBinder expr], expr)
foldForeachBinders = foldBinders getForeachBinder

foldQuantifierBinders ::
  (Show expr, HasBuiltinBinders expr) =>
  Quantifier ->
  GenericBinder expr ->
  expr ->
  ([GenericBinder expr], expr)
foldQuantifierBinders q = foldBinders (getQuantifierBinder q)

foldDeclBinders ::
  (Show expr, HasBasicBinders expr) =>
  LHSBinderCount ->
  expr ->
  ([GenericBinder expr], expr)
foldDeclBinders binderCount expr
  | binderCount == 0 = ([], expr)
  | otherwise = case getLamBinder expr of
      Nothing -> ([], expr)
      Just (binder, body) -> first (binder :) $ foldDeclBinders (binderCount - 1) body

foldBinders ::
  forall expr.
  (Show expr) =>
  (expr -> Maybe (GenericBinder expr, expr)) ->
  GenericBinder expr ->
  expr ->
  ([GenericBinder expr], expr)
foldBinders getBinder leadBinder = go
  where
    go :: expr -> ([GenericBinder expr], expr)
    go expr = case getBinder expr of
      Just (binder, body) | canFold binder && wantsToFold binder -> first (binder :) (go body)
      _ -> ([], expr)

    canFold :: GenericBinder expr -> Bool
    canFold binder =
      visibilityMatches leadBinder binder
        && binderNamingForm leadBinder == binderNamingForm binder

--------------------------------------------------------------------------------
-- Let declarations

type LetBinder expr = (GenericBinder expr, expr)

-- | Collapses consecutative let expressions into a list of let declarations
foldLetBinders :: (HasBasicBinders expr) => expr -> ([LetBinder expr], expr)
foldLetBinders expr = case getLetBinder expr of
  Just (bound, binder, body)
    | wantsToFold binder -> first ((binder, bound) :) (foldLetBinders body)
  _ -> ([], expr)
