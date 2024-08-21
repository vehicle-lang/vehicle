{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Eval where

import Vehicle.Compile.Normalise.Builtin
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Expr.Value

instance NormalisableBuiltin Builtin where
  evalBuiltinApp = evalTypeClassOp evalBuiltinFunction

evalBuiltinFunction :: BuiltinFunction -> EvalBuiltin (Value closure Builtin) m
evalBuiltinFunction b evalApp originalValue args = case b of
  Quantifier {} -> return originalValue
  Not -> return $ evalNot originalValue args
  And -> return $ evalAnd originalValue args
  Or -> return $ evalOr originalValue args
  Neg NegRat -> return $ evalNegRat originalValue args
  Add AddNat -> return $ evalAddNat originalValue args
  Add AddRat -> return $ evalAddRat originalValue args
  Sub SubRat -> return $ evalSubRat originalValue args
  Mul MulNat -> return $ evalMulNat originalValue args
  Mul MulRat -> return $ evalMulRat originalValue args
  Div DivRat -> return $ evalDivRat originalValue args
  PowRat -> return $ evalPowRat originalValue args
  MinRat -> return $ evalMinRat originalValue args
  MaxRat -> return $ evalMaxRat originalValue args
  Equals EqIndex op -> return $ evalEqualsIndex op originalValue args
  Equals EqNat op -> return $ evalEqualsNat op originalValue args
  Equals EqRat op -> return $ evalEqualsRat op originalValue args
  Order OrderIndex op -> return $ evalOrderIndex op originalValue args
  Order OrderNat op -> return $ evalOrderNat op originalValue args
  Order OrderRat op -> return $ evalOrderRat op originalValue args
  FromNat FromNatToIndex -> return $ evalFromNatToIndex originalValue args
  FromNat FromNatToNat -> return $ evalFromNatToNat originalValue args
  FromNat FromNatToRat -> return $ evalFromNatToRat originalValue args
  FromRat FromRatToRat -> return $ evalFromRatToRat originalValue args
  If -> return $ evalIf originalValue args
  At -> return $ evalAt originalValue args
  FoldVector -> evalFoldVector evalApp originalValue args
  FoldList -> evalFoldList (VBuiltinFunction FoldList) evalApp originalValue args
  ZipWithVector -> evalZipWith evalApp originalValue args
  MapList -> evalMapList (VBuiltinFunction MapList) evalApp originalValue args
  MapVector -> evalMapVector evalApp originalValue args
  Indices -> return $ evalIndices (VBuiltin (BuiltinType Index)) originalValue args
  Implies -> return $ evalImplies originalValue args
