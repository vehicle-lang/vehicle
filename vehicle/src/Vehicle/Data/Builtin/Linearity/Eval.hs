{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Linearity.Eval where

import Vehicle.Compile.Error (MonadCompile, compilerDeveloperError)
import Vehicle.Compile.Normalise.Builtin hiding (evalFoldList)
import Vehicle.Compile.Prelude (GenericArg (..), explicit)
import Vehicle.Data.Builtin.Linearity.Core (LinearityBuiltin)
import Vehicle.Data.Builtin.Linearity.Core qualified as L
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Expr.Normalised
import Vehicle.Prelude

instance NormalisableBuiltin LinearityBuiltin where
  evalBuiltinApp = evalTypeClassOp evalBuiltinFunction

evalBuiltinFunction :: (MonadCompile m) => BuiltinFunction -> EvalBuiltin (Value closure LinearityBuiltin) m
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
    notImplemented :: (MonadCompile m) => m a
    notImplemented = compilerDeveloperError $ "Normalisation of " <+> pretty b <+> "at the type-level not yet supported for Linearity system"

-- Need foldList at the type-level to evaluate the Tensor definition
evalFoldList :: EvalBuiltin (Value closure LinearityBuiltin) m
evalFoldList evalApp originalExpr args =
  case args of
    [_a, _b, _c, _f, e, argExpr -> VBuiltin (L.BuiltinConstructor Nil) []] -> return $ argExpr e
    [a, b, c, f, e, argExpr -> VBuiltin (L.BuiltinConstructor Cons) [_, _, _, _, x, xs]] -> do
      let defaultFold = VBuiltin (L.BuiltinFunction FoldList) [a, b, c, f, e, xs]
      r <- evalFoldList evalApp defaultFold [a, b, c, f, e, xs]
      evalApp (argExpr f) [x, explicit r]
    _ -> return originalExpr
