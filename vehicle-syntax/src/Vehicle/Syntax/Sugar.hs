{-# LANGUAGE ViewPatterns #-}

module Vehicle.Syntax.Sugar
  ( BinderFoldTarget (..),
    FoldableBinderType (..),
    FoldableBuiltin (..),
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

class FoldableBuiltin builtin where
  getQuant ::
    Expr binder var builtin ->
    Maybe (Provenance, Quantifier, Binder binder var builtin, Expr binder var builtin)

instance FoldableBuiltin Builtin where
  getQuant = \case
    QuantifierExpr p binder body q -> Just (p, q, binder, body)
    _ -> Nothing

data FoldableBinderType
  = PiFold
  | LamFold
  | QuantFold Quantifier
  deriving (Eq)

data BinderFoldTarget binder var builtin
  = FoldableBinder FoldableBinderType (Binder binder var builtin)
  | FunFold

pattern QuantifierExpr p binder body q <-
  App p (Builtin _ (TypeClassOp (QuantifierTC q))) (ExplicitArg _ (Lam _ binder body) :| [])

foldBinders ::
  forall binder var builtin.
  (Show (Binder binder var builtin), FoldableBuiltin builtin) =>
  BinderFoldTarget binder var builtin ->
  Expr binder var builtin ->
  ([Binder binder var builtin], Expr binder var builtin)
foldBinders foldTarget = go
  where
    go :: Expr binder var builtin -> ([Binder binder var builtin], Expr binder var builtin)
    go expr = do
      let result = case expr of
            Pi p binder body -> processBinder binder body PiFold
            Lam p binder body -> processBinder binder body LamFold
            (getQuant -> Just (p, q, binder, body)) -> processBinder binder body (QuantFold q)
            expr -> Nothing

      case result of
        Nothing -> ([], expr)
        Just (binder, body) -> first (binder :) (go body)

    processBinder ::
      Binder binder var builtin ->
      Expr binder var builtin ->
      FoldableBinderType ->
      Maybe (Binder binder var builtin, Expr binder var builtin)
    processBinder binder body candidateBinderType
      | shouldFold binder candidateBinderType = Just (binder, body)
      | otherwise = Nothing

    shouldFold :: Binder binder var builtin -> FoldableBinderType -> Bool
    shouldFold binder candidateType = case foldTarget of
      FoldableBinder targetType targetBinder ->
        targetType == candidateType
          && canFold binder targetBinder
          && wantsToFold binder
      FunFold -> case candidateType of
        LamFold -> wantsToFold binder
        _ -> False

    canFold :: Binder binder var builtin -> Binder binder var builtin -> Bool
    canFold leadBinder binder =
      visibilityMatches leadBinder binder
        && binderNamingForm leadBinder == binderNamingForm binder

--------------------------------------------------------------------------------
-- Let declarations

type LetBinder binder var builtin = (Binder binder var builtin, Expr binder var builtin)

-- | Collapses consecutative let expressions into a list of let declarations
foldLetBinders :: Expr binder var builtin -> ([LetBinder binder var builtin], Expr binder var builtin)
foldLetBinders = \case
  Let _ bound binder body
    | wantsToFold binder -> first ((binder, bound) :) (foldLetBinders body)
  expr -> ([], expr)
