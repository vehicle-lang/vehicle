{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Polarity.Eval where

import Vehicle.Compile.Normalise.Builtin hiding (evalFoldList)
import Vehicle.Compile.Prelude (GenericArg (..), MonadLogger, explicit)
import Vehicle.Data.Builtin.Polarity.Core (PolarityBuiltin)
import Vehicle.Data.Builtin.Polarity.Core qualified as P
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Expr.Normalised
import Vehicle.Prelude

instance NormalisableBuiltin PolarityBuiltin where
  evalBuiltinApp = evalTypeClassOp evalBuiltinFunction

evalBuiltinFunction :: (MonadLogger m) => BuiltinFunction -> EvalBuiltin (Value closure PolarityBuiltin) m
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
  At -> notImplemented
  FoldVector -> notImplemented
  FoldList -> evalFoldList evalApp originalValue args
  ZipWithVector -> notImplemented
  MapList -> notImplemented
  MapVector -> notImplemented
  Indices -> notImplemented
  Implies -> notImplemented
  where
    notImplemented :: (MonadLogger m) => m a
    notImplemented = developerError $ "Normalisation of " <+> pretty b <+> "at the type-level not yet supported for Polarity system"

-- Need foldList at the type-level to evaluate the Tensor definition
evalFoldList :: EvalBuiltin (Value closure PolarityBuiltin) m
evalFoldList evalApp originalExpr args =
  case args of
    [_a, _b, _c, _f, e, argExpr -> VBuiltin (P.BuiltinConstructor Nil) []] -> return $ argExpr e
    [a, b, c, f, e, argExpr -> VBuiltin (P.BuiltinConstructor Cons) [_, _, _, _, x, xs]] -> do
      let defaultFold = VBuiltin (P.BuiltinFunction FoldList) [a, b, c, f, e, xs]
      r <- evalFoldList evalApp defaultFold [a, b, c, f, e, xs]
      evalApp (argExpr f) [x, explicit r]
    _ -> return originalExpr
