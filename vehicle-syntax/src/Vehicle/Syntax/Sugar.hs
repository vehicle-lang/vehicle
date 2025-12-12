{-# LANGUAGE ViewPatterns #-}

module Vehicle.Syntax.Sugar
  ( HasBasicBinders (..),
    HasBuiltinBinders (..),
    foldPiBinders,
    foldLamBinders,
    foldQuantifierBinders,
    foldForeachBinders,
    foldDeclBinders,
    foldLetBinders,
    foldRecordDef,
    LetBinder,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin
import Vehicle.Syntax.Prelude (developerError)

-- This module deals with all the unfolding and folding of syntactic
-- sugar in the external language. The unfolding is designed so that it should
-- be 100% reversible.

--------------------------------------------------------------------------------
-- Pi/Fun/Forall declarations

class HasBasicBinders expr where
  isUniverse :: expr -> Bool
  getPiBinder :: expr -> Maybe (GenericBinder expr, expr)
  getLamBinder :: expr -> Maybe (GenericBinder expr, expr)
  getLetBinder :: expr -> Maybe (expr, GenericBinder expr, expr)
  getRecord :: expr -> Maybe (GenericRecordFields expr)

class HasBuiltinBinders expr where
  getQuantifierBinder :: Quantifier -> expr -> Maybe (GenericBinder expr, expr)
  getForeachBinder :: expr -> Maybe (GenericBinder expr, expr)

instance HasBasicBinders Expr where
  isUniverse = \case
    Universe {} -> True
    _ -> False

  getPiBinder = \case
    Pi _ binder body -> Just (binder, body)
    _ -> Nothing

  getLamBinder = \case
    Lam _ binder body -> Just (binder, body)
    _ -> Nothing

  getLetBinder = \case
    Let _ value binder body -> Just (value, binder, body)
    _ -> Nothing

  getRecord = \case
    Record _ fields -> Just fields
    _ -> Nothing

instance HasBuiltinBinders Expr where
  getQuantifierBinder q = \case
    App (Builtin _ (TypeClassOp (QuantifierTC q'))) ((argExpr -> Lam _ binder body) :| []) | q == q' -> Just (binder, body)
    _ -> Nothing

  getForeachBinder = \case
    App (Builtin _ (TypeClassOp ForeachTC)) ((argExpr -> Lam _ binder body) :| []) -> Just (binder, body)
    _ -> Nothing

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
    go expr = do
      let result = case getBinder expr of
            Just (binder, body) -> processBinder binder body
            _ -> Nothing

      case result of
        Nothing -> ([], expr)
        Just (binder, body) -> first (binder :) (go body)

    processBinder ::
      GenericBinder expr ->
      expr ->
      Maybe (GenericBinder expr, expr)
    processBinder binder body
      | canFold binder && wantsToFold binder = Just (binder, body)
      | otherwise = Nothing

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

--------------------------------------------------------------------------------
-- Decls

foldRecordDef ::
  (HasBasicBinders expr) =>
  expr ->
  expr ->
  (GenericTelescope expr, GenericRecordFields expr)
foldRecordDef typ body = case (getPiBinder typ, getLamBinder body) of
  (Nothing, Nothing) -> case getRecord body of
    Just fields | isUniverse typ -> ([], fields)
    _ -> developerError "Malformed record definition"
  (Just (piBinder, piBody), Just (_lamBinder, lamBody)) -> do
    let (telescope, fields) = foldRecordDef piBody lamBody
    (piBinder : telescope, fields)
  _ -> developerError "Malformed record definition"
