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
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.BuiltinInterface.Value
import Vehicle.Data.NormalisedExpr
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
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  EvalApp builtin m ->
  builtin ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)
evalBuiltin evalApp b args = do
  let runtimeRelevantArgs = filterRuntimeRelevantArgs args
  let originalExpr = VBuiltin b args
  case getBuiltinFunction b of
    Just f -> evalBuiltinFunction f evalApp originalExpr runtimeRelevantArgs
    _ -> return originalExpr

filterRuntimeRelevantArgs :: WHNFSpine builtin -> [WHNFValue builtin]
filterRuntimeRelevantArgs = mapMaybe getExplicitArg

-----------------------------------------------------------------------------
-- Builtin evaluation

-- | A method for evaluating an application.
-- Although there is only one implementation of this type, it needs to be
-- passed around as an argument to avoid dependency cycles between
-- this module and the module in which the general NBE algorithm lives in.
type EvalApp builtin m =
  WHNFValue builtin ->
  WHNFSpine builtin ->
  m (WHNFValue builtin)

-- | A method for evaluating builtins that takes in an argument allowing the
-- recursive evaluation of applications. that takes in an argument allowing
-- the subsequent further evaluation of applications.
-- Such recursive evaluation is necessary when evaluating higher order
-- functions such as fold, map etc.
type EvalBuiltin builtin m =
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  EvalApp builtin m ->
  WHNFValue builtin ->
  [WHNFValue builtin] ->
  m (WHNFValue builtin)

type EvalSimpleBuiltin builtin =
  forall strategy.
  (HasStandardData builtin) =>
  Value strategy builtin ->
  [Value strategy builtin] ->
  Value strategy builtin

evalBuiltinFunction :: BuiltinFunction -> EvalBuiltin builtin m
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

evalNot :: EvalSimpleBuiltin builtin
evalNot originalExpr = \case
  [VBoolLiteral x] -> VBoolLiteral (not x)
  _ -> originalExpr

evalAnd :: EvalSimpleBuiltin builtin
evalAnd originalExpr = \case
  [VBoolLiteral x, VBoolLiteral y] -> VBoolLiteral (x && y)
  [VBoolLiteral b, y] -> if b then y else VBoolLiteral b
  [x, VBoolLiteral b] -> if b then x else VBoolLiteral b
  _ -> originalExpr

evalOr :: EvalSimpleBuiltin builtin
evalOr originalExpr = \case
  [VBoolLiteral x, VBoolLiteral y] -> VBoolLiteral (x || y)
  [VBoolLiteral b, y] -> if b then VBoolLiteral b else y
  [x, VBoolLiteral b] -> if b then VBoolLiteral b else x
  _ -> originalExpr

evalNeg :: NegDomain -> EvalSimpleBuiltin builtin
evalNeg = \case
  NegInt -> evalNegInt
  NegRat -> evalNegRat

evalNegInt :: EvalSimpleBuiltin builtin
evalNegInt originalExpr = \case
  [VIntLiteral x] -> VIntLiteral (-x)
  _ -> originalExpr

evalNegRat :: EvalSimpleBuiltin builtin
evalNegRat originalExpr = \case
  [VRatLiteral x] -> VRatLiteral (-x)
  _ -> originalExpr

evalAdd :: AddDomain -> EvalSimpleBuiltin builtin
evalAdd = \case
  AddNat -> evalAddNat
  AddInt -> evalAddInt
  AddRat -> evalAddRat

evalAddNat :: EvalSimpleBuiltin builtin
evalAddNat originalExpr = \case
  [VNatLiteral x, VNatLiteral y] -> VNatLiteral (x + y)
  _ -> originalExpr

evalAddInt :: EvalSimpleBuiltin builtin
evalAddInt originalExpr = \case
  [VIntLiteral x, VIntLiteral y] -> VIntLiteral (x + y)
  _ -> originalExpr

evalAddRat :: EvalSimpleBuiltin builtin
evalAddRat originalExpr = \case
  [VRatLiteral x, VRatLiteral y] -> VRatLiteral (x + y)
  _ -> originalExpr

evalSub :: SubDomain -> EvalSimpleBuiltin builtin
evalSub = \case
  SubInt -> evalSubInt
  SubRat -> evalSubRat

evalSubInt :: EvalSimpleBuiltin builtin
evalSubInt originalExpr = \case
  [VIntLiteral x, VIntLiteral y] -> VIntLiteral (x - y)
  _ -> originalExpr

evalSubRat :: EvalSimpleBuiltin builtin
evalSubRat originalExpr = \case
  [VRatLiteral x, VRatLiteral y] -> VRatLiteral (x - y)
  _ -> originalExpr

evalMul :: MulDomain -> EvalSimpleBuiltin builtin
evalMul = \case
  MulNat -> evalMulNat
  MulInt -> evalMulInt
  MulRat -> evalMulRat

evalMulNat :: EvalSimpleBuiltin builtin
evalMulNat originalExpr = \case
  [VNatLiteral x, VNatLiteral y] -> VNatLiteral (x * y)
  _ -> originalExpr

evalMulInt :: EvalSimpleBuiltin builtin
evalMulInt originalExpr = \case
  [VIntLiteral x, VIntLiteral y] -> VIntLiteral (x * y)
  _ -> originalExpr

evalMulRat :: EvalSimpleBuiltin builtin
evalMulRat originalExpr = \case
  [VRatLiteral x, VRatLiteral y] -> VRatLiteral (x * y)
  _ -> originalExpr

evalDiv :: DivDomain -> EvalSimpleBuiltin builtin
evalDiv = \case
  DivRat -> evalDivRat

evalDivRat :: EvalSimpleBuiltin builtin
evalDivRat originalExpr = \case
  [VRatLiteral x, VRatLiteral y] -> VRatLiteral (x / y)
  _ -> originalExpr

evalPowRat :: EvalSimpleBuiltin builtin
evalPowRat originalExpr = \case
  [VRatLiteral x, VIntLiteral y] -> VRatLiteral (x ^^ y)
  _ -> originalExpr

evalMinRat :: EvalSimpleBuiltin builtin
evalMinRat originalExpr = \case
  [VRatLiteral x, VRatLiteral y] -> VRatLiteral (min x y)
  _ -> originalExpr

evalMaxRat :: EvalSimpleBuiltin builtin
evalMaxRat originalExpr = \case
  [VRatLiteral x, VRatLiteral y] -> VRatLiteral (max x y)
  _ -> originalExpr

evalOrder :: OrderDomain -> OrderOp -> EvalSimpleBuiltin builtin
evalOrder = \case
  OrderIndex -> evalOrderIndex
  OrderNat -> evalOrderNat
  OrderInt -> evalOrderInt
  OrderRat -> evalOrderRat

evalOrderIndex :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderIndex op originalExpr = \case
  [VIndexLiteral x, VIndexLiteral y] -> VBoolLiteral (orderOp op x y)
  _ -> originalExpr

evalOrderNat :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderNat op originalExpr = \case
  [VNatLiteral x, VNatLiteral y] -> VBoolLiteral (orderOp op x y)
  _ -> originalExpr

evalOrderInt :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderInt op originalExpr = \case
  [VIntLiteral x, VIntLiteral y] -> VBoolLiteral (orderOp op x y)
  _ -> originalExpr

evalOrderRat :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderRat op originalExpr = \case
  [VRatLiteral x, VRatLiteral y] -> VBoolLiteral (orderOp op x y)
  _ -> originalExpr

evalEquals :: EqualityDomain -> EqualityOp -> EvalSimpleBuiltin builtin
evalEquals = \case
  EqIndex -> evalEqualityIndex
  EqNat -> evalEqualityNat
  EqInt -> evalEqualityInt
  EqRat -> evalEqualityRat

evalEqualityIndex :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityIndex op originalExpr = \case
  [VIndexLiteral x, VIndexLiteral y] -> VBoolLiteral (equalityOp op x y)
  _ -> originalExpr

evalEqualityNat :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityNat op originalExpr = \case
  [VNatLiteral x, VNatLiteral y] -> VBoolLiteral (equalityOp op x y)
  _ -> originalExpr

evalEqualityInt :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityInt op originalExpr = \case
  [VIntLiteral x, VIntLiteral y] -> VBoolLiteral (equalityOp op x y)
  _ -> originalExpr

evalEqualityRat :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityRat op originalExpr = \case
  [VRatLiteral x, VRatLiteral y] -> VBoolLiteral (equalityOp op x y)
  _ -> originalExpr

evalFromNat :: FromNatDomain -> EvalSimpleBuiltin builtin
evalFromNat = \case
  FromNatToIndex -> evalFromNatToIndex
  FromNatToNat -> evalFromNatToNat
  FromNatToInt -> evalFromNatToInt
  FromNatToRat -> evalFromNatToRat

evalFromNatToIndex :: EvalSimpleBuiltin builtin
evalFromNatToIndex originalExpr = \case
  [VNatLiteral x] -> VIndexLiteral x
  _ -> originalExpr

evalFromNatToNat :: EvalSimpleBuiltin builtin
evalFromNatToNat originalExpr = \case
  [x] -> x
  _ -> originalExpr

evalFromNatToInt :: EvalSimpleBuiltin builtin
evalFromNatToInt originalExpr = \case
  [VNatLiteral x] -> VIntLiteral x
  _ -> originalExpr

evalFromNatToRat :: EvalSimpleBuiltin builtin
evalFromNatToRat originalExpr = \case
  [VNatLiteral x] -> VRatLiteral (fromIntegral x)
  _ -> originalExpr

evalFromRat :: FromRatDomain -> EvalSimpleBuiltin builtin
evalFromRat = \case
  FromRatToRat -> evalFromRatToRat

evalFromRatToRat :: EvalSimpleBuiltin builtin
evalFromRatToRat originalExpr = \case
  [x] -> x
  _ -> originalExpr

evalIf :: EvalSimpleBuiltin builtin
evalIf originalExpr = \case
  [VBoolLiteral True, e1, _e2] -> e1
  [VBoolLiteral False, _e1, e2] -> e2
  _ -> originalExpr

evalAt :: EvalSimpleBuiltin builtin
evalAt originalExpr = \case
  [VVecLiteral xs, VIndexLiteral i] -> case xs !!? fromIntegral i of
    Nothing -> developerError $ "out of bounds error:" <+> pretty (length xs) <+> "<=" <+> pretty i
    Just xsi -> argExpr xsi
  _ -> originalExpr

evalFoldList :: EvalBuiltin builtin m
evalFoldList evalApp originalExpr = \case
  [_f, e, VNil] -> return e
  [f, e, VCons x xs] -> do
    let defaultFold = VBuiltin (mkBuiltinFunction FoldList) [Arg mempty Explicit Relevant f, Arg mempty Explicit Relevant e, xs]
    r <- evalFoldList evalApp defaultFold [f, e, argExpr xs]
    evalApp f [x, Arg mempty Explicit Relevant r]
  _ -> return originalExpr

evalFoldVector :: EvalBuiltin builtin m
evalFoldVector evalApp originalExpr = \case
  [f, e, VVecLiteral xs] -> foldrM f' e xs
    where
      f' x r =
        evalApp
          f
          [ x,
            Arg mempty Explicit Relevant r
          ]
  _ -> return originalExpr

evalZipWith :: EvalBuiltin builtin m
evalZipWith evalApp originalExpr = \case
  [f, VVecLiteral xs, VVecLiteral ys] ->
    mkVLVec <$> zipWithM f' xs ys
    where
      f' x y =
        evalApp
          f
          [ x,
            y
          ]
  _ -> return originalExpr

evalMapList :: EvalBuiltin builtin m
evalMapList evalApp originalExpr = \case
  [_f, e@VNil] ->
    return e
  [f, VCons x xs] -> do
    fx <- evalApp f [x]
    let defaultMap = VBuiltin (mkBuiltinFunction MapList) [Arg mempty Explicit Relevant f, xs]
    fxs <- evalMapList evalApp defaultMap [f, argExpr xs]
    return $ VBuiltin (mkBuiltinConstructor Cons) (Arg mempty Explicit Relevant <$> [fx, fxs])
  _ -> return originalExpr

evalMapVector :: EvalBuiltin builtin m
evalMapVector evalApp originalExpr = \case
  [f, VVecLiteral xs] ->
    mkVLVec <$> traverse f' xs
    where
      f' x = evalApp f [x]
  _ -> return originalExpr

evalIndices :: EvalSimpleBuiltin builtin
evalIndices originalExpr = \case
  [VNatLiteral n] -> mkVLVec (fmap VIndexLiteral [0 .. n - 1])
  _ -> originalExpr

-----------------------------------------------------------------------------
-- Derived

-- TODO define in terms of language. The problem is the polarity checking...
evalImplies :: EvalSimpleBuiltin builtin
evalImplies originalExpr = \case
  [e1, e2] -> do
    let defaultNot = VBuiltinFunction Not [Arg mempty Explicit Relevant e1]
    let ne1 = evalNot defaultNot [e1]
    let defaultOr = VBuiltinFunction Or [Arg mempty Explicit Relevant ne1, Arg mempty Explicit Relevant e2]
    evalOr defaultOr [ne1, e2]
  _ -> originalExpr
