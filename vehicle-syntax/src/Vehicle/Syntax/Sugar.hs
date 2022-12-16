{-# LANGUAGE ScopedTypeVariables #-}

module Vehicle.Syntax.Sugar
  ( BinderFoldTarget (..),
    FoldableBinderType (..),
    foldBinders,
    foldLetBinders,
    LetBinder,
  )
where

import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Vehicle.Syntax.AST

-- This module deals with all the unfolding and folding of syntactic
-- sugar in the external language. The unfolding is designed so that it should
-- be 100% reversible.

--------------------------------------------------------------------------------
-- Pi/Fun/Forall declarations

data FoldableBinderType
  = PiFold
  | LamFold
  | ForeachFold
  | QuantFold Quantifier
  | QuantInFold Quantifier
  deriving (Eq)

data BinderFoldTarget binder var
  = FoldableBinder FoldableBinderType (Binder binder var)
  | FunFold

pattern ForeachExpr p binder body <-
  App p (Builtin _ Foreach) (ExplicitArg _ (Lam _ binder body) :| [])

pattern QuantifierExpr p binder body q <-
  App p (Builtin _ (TypeClassOp (QuantifierTC q))) (ExplicitArg _ (Lam _ binder body) :| [])

pattern QuantifierInExpr p binder body q cont <-
  App
    p
    (Builtin _ (TypeClassOp (QuantifierInTC q)))
    (ExplicitArg _ (Lam _ binder body) :| [ExplicitArg _ cont])

foldBinders ::
  forall binder var.
  Show (Binder binder var) =>
  BinderFoldTarget binder var ->
  Expr binder var ->
  ([Binder binder var], Expr binder var)
foldBinders foldTarget = go
  where
    go :: Expr binder var -> ([Binder binder var], Expr binder var)
    go expr = do
      let result = case expr of
            Pi p binder body -> processBinder binder body PiFold
            Lam p binder body -> processBinder binder body LamFold
            ForeachExpr p binder body -> processBinder binder body ForeachFold
            QuantifierExpr p binder body q -> processBinder binder body (QuantFold q)
            QuantifierInExpr p binder body q _ -> processBinder binder body (QuantInFold q)
            expr -> Nothing

      case result of
        Nothing -> ([], expr)
        Just (binder, body) -> first (binder :) (go body)

    processBinder ::
      Binder binder var ->
      Expr binder var ->
      FoldableBinderType ->
      Maybe (Binder binder var, Expr binder var)
    processBinder binder body candidateBinderType
      | shouldFold binder candidateBinderType = Just (binder, body)
      | otherwise = Nothing

    shouldFold :: Binder binder var -> FoldableBinderType -> Bool
    shouldFold binder candidateType = case foldTarget of
      FoldableBinder targetType targetBinder ->
        targetType == candidateType
          && canFold binder targetBinder
          && wantsToFold binder
      FunFold -> case candidateType of
        LamFold -> wantsToFold binder
        _ -> False

    canFold :: Binder binder var -> Binder binder var -> Bool
    canFold leadBinder binder =
      visibilityMatches leadBinder binder
        && binderNamingForm leadBinder == binderNamingForm binder

--------------------------------------------------------------------------------
-- Let declarations

type LetBinder binder var = (Binder binder var, Expr binder var)

-- | Collapses consecutative let expressions into a list of let declarations
foldLetBinders :: Expr binder var -> ([LetBinder binder var], Expr binder var)
foldLetBinders = \case
  Let _ bound binder body
    | wantsToFold binder -> first ((binder, bound) :) (foldLetBinders body)
  expr -> ([], expr)
