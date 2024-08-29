module Vehicle.Syntax.Sugar
  ( BinderType (..),
    HasBinders (..),
    foldBinders,
    foldDeclBinders,
    foldLetBinders,
    LetBinder,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Vehicle.Syntax.AST
import Vehicle.Syntax.Builtin

-- This module deals with all the unfolding and folding of syntactic
-- sugar in the external language. The unfolding is designed so that it should
-- be 100% reversible.

--------------------------------------------------------------------------------
-- Pi/Fun/Forall declarations

data BinderType
  = PiBinder
  | LamBinder
  | QuantifierBinder Quantifier
  deriving (Eq)

class HasBinders expr where
  getBinder :: expr -> Maybe (BinderType, GenericBinder expr, expr)
  getLetBinder :: expr -> Maybe (expr, GenericBinder expr, expr)

instance HasBinders Expr where
  getBinder = \case
    Pi _ binder body -> Just (PiBinder, binder, body)
    Lam _ binder body -> Just (LamBinder, binder, body)
    QuantifierExpr binder body q -> Just (QuantifierBinder q, binder, body)
    _ -> Nothing

  getLetBinder = \case
    Let _ value binder body -> Just (value, binder, body)
    _ -> Nothing

data BinderFoldTarget expr
  = FoldableBinder BinderType (GenericBinder expr)
  | FunFold

pattern QuantifierExpr :: Binder -> Expr -> Quantifier -> Expr
pattern QuantifierExpr binder body q <-
  App (Builtin _ (TypeClassOp (QuantifierTC q))) (RelevantExplicitArg _ (Lam _ binder body) :| [])

foldBinders :: (Show expr, HasBinders expr) => BinderType -> GenericBinder expr -> expr -> ([GenericBinder expr], expr)
foldBinders binderType binder = foldBinders' (FoldableBinder binderType binder)

foldDeclBinders :: (Show expr, HasBinders expr) => expr -> ([GenericBinder expr], expr)
foldDeclBinders = foldBinders' FunFold

foldBinders' ::
  forall expr.
  (Show expr, HasBinders expr) =>
  BinderFoldTarget expr ->
  expr ->
  ([GenericBinder expr], expr)
foldBinders' foldTarget = go
  where
    go :: expr -> ([GenericBinder expr], expr)
    go expr = do
      let result = case getBinder expr of
            Just (binderType, binder, body) -> processBinder binder body binderType
            _ -> Nothing

      case result of
        Nothing -> ([], expr)
        Just (binder, body) -> first (binder :) (go body)

    processBinder ::
      GenericBinder expr ->
      expr ->
      BinderType ->
      Maybe (GenericBinder expr, expr)
    processBinder binder body candidateBinderType
      | shouldFold binder candidateBinderType = Just (binder, body)
      | otherwise = Nothing

    shouldFold :: GenericBinder expr -> BinderType -> Bool
    shouldFold binder candidateType = case foldTarget of
      FoldableBinder targetType targetBinder ->
        targetType == candidateType
          && canFold binder targetBinder
          && wantsToFold binder
      FunFold -> case candidateType of
        LamBinder -> wantsToFold binder
        _ -> False

    canFold :: GenericBinder expr -> GenericBinder expr -> Bool
    canFold leadBinder binder =
      visibilityMatches leadBinder binder
        && binderNamingForm leadBinder == binderNamingForm binder

--------------------------------------------------------------------------------
-- Let declarations

type LetBinder expr = (GenericBinder expr, expr)

-- | Collapses consecutative let expressions into a list of let declarations
foldLetBinders :: (HasBinders expr) => expr -> ([LetBinder expr], expr)
foldLetBinders expr = case getLetBinder expr of
  Just (bound, binder, body)
    | wantsToFold binder -> first ((binder, bound) :) (foldLetBinders body)
  _ -> ([], expr)
