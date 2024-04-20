{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.Builtin
  ( EvalSimpleBuiltin,
    evalBuiltin,
    evalMulRat,
    evalAddRat,
    evalSubRat,
    evalDivRat,
    evalNegRat,
    evalEqualityRat,
    evalEquals,
    evalOrderRat,
    evalOrder,
    evalAnd,
    evalOr,
    evalAt,
    evalFoldVector,
    evalMapVector,
    evalZipWith,
    evalIndices,
  )
where

import Control.Monad (zipWithM)
import Data.Foldable (foldrM)
import Data.Maybe (mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Data.BuiltinInterface.ASTInterface
import Vehicle.Syntax.Builtin

-- Okay so the important thing to remember about this module is that we have
-- a variety of different typing schemes for builtins (standard, polarity,
-- linearity etc.). Normalisation needs to work for all of these, and
-- therefore we can't guarantee what the implicit and instance arguments are
-- going to be for a given builtin. However, explicit arguments are always
-- the same in every type system.

-- Therefore this can be viewed as a type of runtime irrelevance, where only
-- the explicit arguments are runtime relevant. This notion isn't made
-- explicit in the code below. Maybe there's a nice way of doing so?

-----------------------------------------------------------------------------
-- Main method

evalBuiltin ::
  (MonadCompile m, HasStandardDataExpr expr) =>
  EvalApp expr m ->
  expr ->
  m expr
evalBuiltin evalApp expr = case getFunction expr of
  Nothing -> return expr
  Just (_, f, args) -> do
    let runtimeRelevantArgs = filterRuntimeRelevantArgs args
    evalBuiltinFunction f evalApp expr runtimeRelevantArgs

filterRuntimeRelevantArgs :: [GenericArg expr] -> [expr]
filterRuntimeRelevantArgs = mapMaybe getExplicitArg

-----------------------------------------------------------------------------
-- Builtin evaluation

-- | A method for evaluating an application.
-- Although there is only one implementation of this type, it needs to be
-- passed around as an argument to avoid dependency cycles between
-- this module and the module in which the general NBE algorithm lives in.
type EvalApp expr m = expr -> [GenericArg expr] -> m expr

-- | A method for evaluating builtins that takes in an argument allowing the
-- recursive evaluation of applications. that takes in an argument allowing
-- the subsequent further evaluation of applications.
-- Such recursive evaluation is necessary when evaluating higher order
-- functions such as fold, map etc.
type EvalBuiltin expr m =
  (MonadCompile m, HasStandardDataExpr expr) =>
  EvalApp expr m ->
  expr ->
  [expr] ->
  m expr

type EvalSimpleBuiltin expr =
  (HasStandardDataExpr expr) =>
  expr ->
  [expr] ->
  expr

evalBuiltinFunction :: BuiltinFunction -> EvalBuiltin expr m
evalBuiltinFunction b evalApp originalValue args = case b of
  Quantifier {} -> return originalValue
  Optimise {} -> return originalValue
  Not -> return $ evalNot originalValue args
  And -> return $ evalAnd originalValue args
  Or -> return $ evalOr originalValue args
  Neg dom -> return $ evalNeg dom originalValue args
  Add dom -> return $ evalAdd dom originalValue args
  Sub dom -> return $ evalSub dom originalValue args
  Mul dom -> return $ evalMul dom originalValue args
  Div dom -> return $ evalDiv dom originalValue args
  PowRat -> return $ evalPowRat originalValue args
  MinRat -> return $ evalMinRat originalValue args
  MaxRat -> return $ evalMaxRat originalValue args
  Equals dom op -> return $ evalEquals dom op originalValue args
  Order dom op -> return $ evalOrder dom op originalValue args
  If -> return $ evalIf originalValue args
  At -> return $ evalAt originalValue args
  FoldVector -> evalFoldVector evalApp originalValue args
  FoldList -> evalFoldList evalApp originalValue args
  ZipWithVector -> evalZipWith evalApp originalValue args
  MapList -> evalMapList evalApp originalValue args
  MapVector -> evalMapVector evalApp originalValue args
  FromNat dom -> return $ evalFromNat dom originalValue args
  FromRat dom -> return $ evalFromRat dom originalValue args
  Indices -> return $ evalIndices originalValue args
  Implies -> return $ evalImplies originalValue args

-----------------------------------------------------------------------------
-- Individual builtin evaluation
-----------------------------------------------------------------------------

evalNot :: EvalSimpleBuiltin expr
evalNot originalExpr = \case
  [IBoolLiteral _ x] -> IBoolLiteral mempty (not x)
  _ -> originalExpr

evalAnd :: EvalSimpleBuiltin expr
evalAnd originalExpr = \case
  [IBoolLiteral _ x, IBoolLiteral _ y] -> IBoolLiteral mempty (x && y)
  [IBoolLiteral _ b, y] -> if b then y else IBoolLiteral mempty b
  [x, IBoolLiteral _ b] -> if b then x else IBoolLiteral mempty b
  _ -> originalExpr

evalOr :: EvalSimpleBuiltin expr
evalOr originalExpr = \case
  [IBoolLiteral _ x, IBoolLiteral _ y] -> IBoolLiteral mempty (x || y)
  [IBoolLiteral _ b, y] -> if b then IBoolLiteral mempty b else y
  [x, IBoolLiteral _ b] -> if b then IBoolLiteral mempty b else x
  _ -> originalExpr

evalNeg :: NegDomain -> EvalSimpleBuiltin expr
evalNeg = \case
  NegRat -> evalNegRat

evalNegRat :: EvalSimpleBuiltin expr
evalNegRat originalExpr = \case
  [IRatLiteral _ x] -> IRatLiteral mempty (-x)
  _ -> originalExpr

evalAdd :: AddDomain -> EvalSimpleBuiltin expr
evalAdd = \case
  AddNat -> evalAddNat
  AddRat -> evalAddRat

evalAddNat :: EvalSimpleBuiltin expr
evalAddNat originalExpr = \case
  [INatLiteral _ x, INatLiteral _ y] -> INatLiteral mempty (x + y)
  _ -> originalExpr

evalAddRat :: EvalSimpleBuiltin expr
evalAddRat originalExpr = \case
  [IRatLiteral _ x, IRatLiteral _ y] -> IRatLiteral mempty (x + y)
  _ -> originalExpr

evalSub :: SubDomain -> EvalSimpleBuiltin expr
evalSub = \case
  SubRat -> evalSubRat

evalSubRat :: EvalSimpleBuiltin expr
evalSubRat originalExpr = \case
  [IRatLiteral _ x, IRatLiteral _ y] -> IRatLiteral mempty (x - y)
  _ -> originalExpr

evalMul :: MulDomain -> EvalSimpleBuiltin expr
evalMul = \case
  MulNat -> evalMulNat
  MulRat -> evalMulRat

evalMulNat :: EvalSimpleBuiltin expr
evalMulNat originalExpr = \case
  [INatLiteral _ x, INatLiteral _ y] -> INatLiteral mempty (x * y)
  _ -> originalExpr

evalMulRat :: EvalSimpleBuiltin expr
evalMulRat originalExpr = \case
  [IRatLiteral _ x, IRatLiteral _ y] -> IRatLiteral mempty (x * y)
  _ -> originalExpr

evalDiv :: DivDomain -> EvalSimpleBuiltin expr
evalDiv = \case
  DivRat -> evalDivRat

evalDivRat :: EvalSimpleBuiltin expr
evalDivRat originalExpr = \case
  [IRatLiteral _ x, IRatLiteral _ y] -> IRatLiteral mempty (x / y)
  _ -> originalExpr

evalPowRat :: EvalSimpleBuiltin expr
evalPowRat originalExpr = \case
  [IRatLiteral _ x, INatLiteral _ y] -> IRatLiteral mempty (x ^^ y)
  _ -> originalExpr

evalMinRat :: EvalSimpleBuiltin expr
evalMinRat originalExpr = \case
  [IRatLiteral _ x, IRatLiteral _ y] -> IRatLiteral mempty (min x y)
  _ -> originalExpr

evalMaxRat :: EvalSimpleBuiltin expr
evalMaxRat originalExpr = \case
  [IRatLiteral _ x, IRatLiteral _ y] -> IRatLiteral mempty (max x y)
  _ -> originalExpr

evalOrder :: OrderDomain -> OrderOp -> EvalSimpleBuiltin expr
evalOrder = \case
  OrderIndex -> evalOrderIndex
  OrderNat -> evalOrderNat
  OrderRat -> evalOrderRat

evalOrderIndex :: OrderOp -> EvalSimpleBuiltin expr
evalOrderIndex op originalExpr = \case
  [IIndexLiteral _ x, IIndexLiteral _ y] -> IBoolLiteral mempty (orderOp op x y)
  _ -> originalExpr

evalOrderNat :: OrderOp -> EvalSimpleBuiltin expr
evalOrderNat op originalExpr = \case
  [INatLiteral _ x, INatLiteral _ y] -> IBoolLiteral mempty (orderOp op x y)
  _ -> originalExpr

evalOrderRat :: OrderOp -> EvalSimpleBuiltin expr
evalOrderRat op originalExpr = \case
  [IRatLiteral _ x, IRatLiteral _ y] -> IBoolLiteral mempty (orderOp op x y)
  _ -> originalExpr

evalEquals :: EqualityDomain -> EqualityOp -> EvalSimpleBuiltin expr
evalEquals = \case
  EqIndex -> evalEqualityIndex
  EqNat -> evalEqualityNat
  EqRat -> evalEqualityRat

evalEqualityIndex :: EqualityOp -> EvalSimpleBuiltin expr
evalEqualityIndex op originalExpr = \case
  [IIndexLiteral _ x, IIndexLiteral _ y] -> IBoolLiteral mempty (equalityOp op x y)
  _ -> originalExpr

evalEqualityNat :: EqualityOp -> EvalSimpleBuiltin expr
evalEqualityNat op originalExpr = \case
  [INatLiteral _ x, INatLiteral _ y] -> IBoolLiteral mempty (equalityOp op x y)
  _ -> originalExpr

evalEqualityRat :: EqualityOp -> EvalSimpleBuiltin expr
evalEqualityRat op originalExpr = \case
  [IRatLiteral _ x, IRatLiteral _ y] -> IBoolLiteral mempty (equalityOp op x y)
  _ -> originalExpr

evalFromNat :: FromNatDomain -> EvalSimpleBuiltin expr
evalFromNat = \case
  FromNatToIndex -> evalFromNatToIndex
  FromNatToNat -> evalFromNatToNat
  FromNatToRat -> evalFromNatToRat

evalFromNatToIndex :: EvalSimpleBuiltin expr
evalFromNatToIndex originalExpr = \case
  [INatLiteral _ x] -> IIndexLiteral mempty x
  _ -> originalExpr

evalFromNatToNat :: EvalSimpleBuiltin expr
evalFromNatToNat originalExpr = \case
  [x] -> x
  _ -> originalExpr

evalFromNatToRat :: EvalSimpleBuiltin expr
evalFromNatToRat originalExpr = \case
  [INatLiteral _ x] -> IRatLiteral mempty (fromIntegral x)
  _ -> originalExpr

evalFromRat :: FromRatDomain -> EvalSimpleBuiltin expr
evalFromRat = \case
  FromRatToRat -> evalFromRatToRat

evalFromRatToRat :: EvalSimpleBuiltin expr
evalFromRatToRat originalExpr = \case
  [x] -> x
  _ -> originalExpr

evalIf :: EvalSimpleBuiltin expr
evalIf originalExpr = \case
  [IBoolLiteral _ True, e1, _e2] -> e1
  [IBoolLiteral _ False, _e1, e2] -> e2
  _ -> originalExpr

evalAt :: EvalSimpleBuiltin expr
evalAt originalExpr = \case
  [IVecLiteral xs, IIndexLiteral _ i] -> case xs !!? fromIntegral i of
    Nothing -> developerError $ "out of bounds error:" <+> pretty (length xs) <+> "<=" <+> pretty i
    Just xsi -> argExpr xsi
  _ -> originalExpr

evalFoldList :: EvalBuiltin expr m
evalFoldList evalApp originalExpr = \case
  [_f, e, INil] -> return e
  [f, e, ICons x xs] -> do
    let defaultFold = mkFunction mempty FoldList [Arg mempty Explicit Relevant f, Arg mempty Explicit Relevant e, xs]
    r <- evalFoldList evalApp defaultFold [f, e, argExpr xs]
    evalApp f [x, Arg mempty Explicit Relevant r]
  _ -> return originalExpr

evalFoldVector :: EvalBuiltin expr m
evalFoldVector evalApp originalExpr = \case
  [f, e, IVecLiteral xs] -> foldrM f' e xs
    where
      f' x r =
        evalApp
          f
          [ x,
            Arg mempty Explicit Relevant r
          ]
  _ -> return originalExpr

evalZipWith :: EvalBuiltin expr m
evalZipWith evalApp originalExpr = \case
  [f, IVecLiteral xs, IVecLiteral ys] ->
    mkVecExpr <$> zipWithM f' xs ys
    where
      f' x y =
        evalApp
          f
          [ x,
            y
          ]
  _ -> return originalExpr

evalMapList :: EvalBuiltin expr m
evalMapList evalApp originalExpr = \case
  [_f, e@INil] -> return e
  [f, ICons x xs] -> do
    fx <- evalApp f [x]
    let defaultMap = mkFunction mempty MapList [Arg mempty Explicit Relevant f, xs]
    fxs <- evalMapList evalApp defaultMap [f, argExpr xs]
    return $ mkConstructor mempty Cons (Arg mempty Explicit Relevant <$> [fx, fxs])
  _ -> return originalExpr

evalMapVector :: EvalBuiltin expr m
evalMapVector evalApp originalExpr = \case
  [f, IVecLiteral xs] ->
    mkVecExpr <$> traverse f' xs
    where
      f' x = evalApp f [x]
  _ -> return originalExpr

evalIndices :: EvalSimpleBuiltin expr
evalIndices originalExpr = \case
  [INatLiteral _ n] -> mkVecExpr (fmap (IIndexLiteral mempty) [0 .. n - 1])
  _ -> originalExpr

-----------------------------------------------------------------------------
-- Derived

-- TODO define in terms of language. The problem is the polarity checking...
evalImplies :: EvalSimpleBuiltin expr
evalImplies originalExpr = \case
  [e1, e2] -> do
    let ne1 = evalNot (INot e1) [e1]
    evalOr (IOr ne1 e2) [ne1, e2]
  _ -> originalExpr
