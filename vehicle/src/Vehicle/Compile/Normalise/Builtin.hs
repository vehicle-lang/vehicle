{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.Builtin where

import Control.Monad (zipWithM)
import Data.Foldable (foldrM)
import Data.Maybe (fromMaybe, mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Data.BuiltinInterface
import Vehicle.Data.NormalisedExpr
import Vehicle.Syntax.Builtin

type EvalApp builtin m = Value builtin -> Spine builtin -> m (Value builtin)

type ForceArg builtin m = VArg builtin -> m (VArg builtin, (Bool, MetaSet))

type NormalisableBuiltin builtin = HasStandardData builtin

-----------------------------------------------------------------------------
-- Indvidual builtins

evalBuiltin ::
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  EvalApp builtin m ->
  builtin ->
  Spine builtin ->
  m (Value builtin)
evalBuiltin evalApp b args = case getBuiltinFunction b of
  Just f -> do
    let result = evalBuiltinFunction evalApp f (mapMaybe getExplicitArg args)
    fromMaybe (return $ VBuiltin b args) result
  Nothing -> return $ VBuiltin b args

evalBuiltinFunction ::
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  EvalApp builtin m ->
  BuiltinFunction ->
  [Value builtin] ->
  Maybe (m (Value builtin))
evalBuiltinFunction evalApp b args
  | isDerived b = evalImplies args
  | otherwise = case b of
      Quantifier {} -> Nothing
      Not -> return <$> evalNot args
      And -> return <$> evalAnd args
      Or -> return <$> evalOr args
      Neg dom -> return <$> evalNeg dom args
      Add dom -> return <$> evalAdd dom args
      Sub dom -> return <$> evalSub dom args
      Mul dom -> return <$> evalMul dom args
      Div dom -> return <$> evalDiv dom args
      PowRat -> return <$> evalPowRat args
      MinRat -> return <$> evalMinRat args
      MaxRat -> return <$> evalMaxRat args
      Equals dom op -> return <$> evalEquals dom op args
      Order dom op -> return <$> evalOrder dom op args
      If -> return <$> evalIf args
      At -> return <$> evalAt args
      Fold dom -> evalFold dom evalApp args
      ZipWithVector -> evalZipWith evalApp args
      MapList -> evalMapList evalApp args
      MapVector -> evalMapVector evalApp args
      FromNat dom -> return <$> evalFromNat dom args
      FromRat dom -> return <$> evalFromRat dom args
      Indices -> return <$> evalIndices args
      Implies -> Just $ compilerDeveloperError $ "Found derived builtin" <+> pretty b
      Optimise {} -> Nothing
      Ann -> return <$> evalAnn args

isDerived :: BuiltinFunction -> Bool
isDerived = \case
  Implies {} -> True
  _ -> False

type EvalBuiltin builtin m =
  (MonadCompile m, PrintableBuiltin builtin, HasStandardData builtin) =>
  [Value builtin] ->
  Maybe (m (Value builtin))

type EvalSimpleBuiltin builtin =
  (HasStandardData builtin) =>
  [Value builtin] ->
  Maybe (Value builtin)

evalNot :: EvalSimpleBuiltin builtin
evalNot e = case e of
  [VBoolLiteral x] -> Just $ VBoolLiteral (not x)
  _ -> Nothing

evalAnd :: EvalSimpleBuiltin builtin
evalAnd = \case
  [VBoolLiteral x, VBoolLiteral y] -> Just $ VBoolLiteral (x && y)
  _ -> Nothing

evalOr :: EvalSimpleBuiltin builtin
evalOr = \case
  [VBoolLiteral x, VBoolLiteral y] -> Just $ VBoolLiteral (x || y)
  _ -> Nothing

evalNeg :: NegDomain -> EvalSimpleBuiltin builtin
evalNeg = \case
  NegInt -> evalNegInt
  NegRat -> evalNegRat

evalNegInt :: EvalSimpleBuiltin builtin
evalNegInt = \case
  [VIntLiteral x] -> Just $ VIntLiteral (-x)
  _ -> Nothing

evalNegRat :: EvalSimpleBuiltin builtin
evalNegRat = \case
  [VRatLiteral x] -> Just $ VRatLiteral (-x)
  _ -> Nothing

evalAdd :: AddDomain -> EvalSimpleBuiltin builtin
evalAdd = \case
  AddNat -> evalAddNat
  AddInt -> evalAddInt
  AddRat -> evalAddRat

evalAddNat :: EvalSimpleBuiltin builtin
evalAddNat = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VNatLiteral (x + y)
  _ -> Nothing

evalAddInt :: EvalSimpleBuiltin builtin
evalAddInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VIntLiteral (x + y)
  _ -> Nothing

evalAddRat :: EvalSimpleBuiltin builtin
evalAddRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x + y)
  _ -> Nothing

evalSub :: SubDomain -> EvalSimpleBuiltin builtin
evalSub = \case
  SubInt -> evalSubInt
  SubRat -> evalSubRat

evalSubInt :: EvalSimpleBuiltin builtin
evalSubInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VIntLiteral (x - y)
  _ -> Nothing

evalSubRat :: EvalSimpleBuiltin builtin
evalSubRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x - y)
  _ -> Nothing

evalMul :: MulDomain -> EvalSimpleBuiltin builtin
evalMul = \case
  MulNat -> evalMulNat
  MulInt -> evalMulInt
  MulRat -> evalMulRat

evalMulNat :: EvalSimpleBuiltin builtin
evalMulNat = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VNatLiteral (x * y)
  _ -> Nothing

evalMulInt :: EvalSimpleBuiltin builtin
evalMulInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VIntLiteral (x * y)
  _ -> Nothing

evalMulRat :: EvalSimpleBuiltin builtin
evalMulRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x * y)
  _ -> Nothing

evalDiv :: DivDomain -> EvalSimpleBuiltin builtin
evalDiv = \case
  DivRat -> evalDivRat

evalDivRat :: EvalSimpleBuiltin builtin
evalDivRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x / y)
  _ -> Nothing

evalPowRat :: EvalSimpleBuiltin builtin
evalPowRat = \case
  [VRatLiteral x, VIntLiteral y] -> Just $ VRatLiteral (x ^^ y)
  _ -> Nothing

evalMinRat :: EvalSimpleBuiltin builtin
evalMinRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (min x y)
  _ -> Nothing

evalMaxRat :: EvalSimpleBuiltin builtin
evalMaxRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (max x y)
  _ -> Nothing

evalOrder :: OrderDomain -> OrderOp -> EvalSimpleBuiltin builtin
evalOrder = \case
  OrderIndex -> evalOrderIndex
  OrderNat -> evalOrderNat
  OrderInt -> evalOrderInt
  OrderRat -> evalOrderRat

evalOrderIndex :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderIndex op = \case
  [VIndexLiteral x, VIndexLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderNat :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderNat op = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderInt :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderInt op = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderRat :: OrderOp -> EvalSimpleBuiltin builtin
evalOrderRat op = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalEquals :: EqualityDomain -> EqualityOp -> EvalSimpleBuiltin builtin
evalEquals = \case
  EqIndex -> evalEqualityIndex
  EqNat -> evalEqualityNat
  EqInt -> evalEqualityInt
  EqRat -> evalEqualityRat

evalEqualityIndex :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityIndex op = \case
  [VIndexLiteral x, VIndexLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityNat :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityNat op = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityInt :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityInt op = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityRat :: EqualityOp -> EvalSimpleBuiltin builtin
evalEqualityRat op = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalFromNat :: FromNatDomain -> EvalSimpleBuiltin builtin
evalFromNat = \case
  FromNatToIndex -> evalFromNatToIndex
  FromNatToNat -> evalFromNatToNat
  FromNatToInt -> evalFromNatToInt
  FromNatToRat -> evalFromNatToRat

evalFromNatToIndex :: EvalSimpleBuiltin builtin
evalFromNatToIndex = \case
  [VNatLiteral x] -> Just $ VIndexLiteral x
  _ -> Nothing

evalFromNatToNat :: EvalSimpleBuiltin builtin
evalFromNatToNat = \case
  [x] -> Just x
  _ -> Nothing

evalFromNatToInt :: EvalSimpleBuiltin builtin
evalFromNatToInt = \case
  [VNatLiteral x] -> Just $ VIntLiteral x
  _ -> Nothing

evalFromNatToRat :: EvalSimpleBuiltin builtin
evalFromNatToRat = \case
  [VNatLiteral x] -> Just $ VRatLiteral (fromIntegral x)
  _ -> Nothing

evalFromRat :: FromRatDomain -> EvalSimpleBuiltin builtin
evalFromRat = \case
  FromRatToRat -> evalFromRatToRat

evalFromRatToRat :: EvalSimpleBuiltin builtin
evalFromRatToRat = \case
  [x] -> Just x
  _ -> Nothing

evalIf :: EvalSimpleBuiltin builtin
evalIf = \case
  [VBoolLiteral True, e1, _e2] -> Just e1
  [VBoolLiteral False, _e1, e2] -> Just e2
  _ -> Nothing

evalAt :: EvalSimpleBuiltin builtin
evalAt = \case
  [VVecLiteral xs, VIndexLiteral i] -> Just $ case xs !!? fromIntegral i of
    Nothing -> developerError $ "out of bounds error:" <+> pretty (length xs) <+> "<=" <+> pretty i
    Just xsi -> argExpr xsi
  _ -> Nothing

evalConsVector :: EvalSimpleBuiltin builtin
evalConsVector = \case
  [x, VVecLiteral xs] -> Just $ mkVLVec (x : fmap argExpr xs)
  _ -> Nothing

evalFold ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  FoldDomain ->
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalFold = \case
  FoldList -> evalFoldList
  FoldVector -> evalFoldVector

evalFoldList ::
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalFoldList evalApp = \case
  [_f, e, VNil] ->
    Just $ return e
  [f, e, VCons [x, xs]] ->
    Just $ do
      let defaultFold = return $ VBuiltin (mkBuiltinFunction (Fold FoldList)) [Arg mempty Explicit Relevant f, Arg mempty Explicit Relevant e, xs]
      r <- fromMaybe defaultFold $ evalFoldList evalApp [f, e, argExpr xs]
      evalApp f [x, Arg mempty Explicit Relevant r]
  _ -> Nothing

evalFoldVector ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalFoldVector evalApp = \case
  [f, e, VVecLiteral xs] ->
    Just $ foldrM f' e xs
    where
      f' x r =
        evalApp
          f
          [ x,
            Arg mempty Explicit Relevant r
          ]
  _ -> Nothing

evalZipWith ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalZipWith evalApp = \case
  [f, VVecLiteral xs, VVecLiteral ys] ->
    Just $ mkVLVec <$> zipWithM f' xs ys
    where
      f' x y =
        evalApp
          f
          [ x,
            y
          ]
  _ -> Nothing

evalMapList ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalMapList evalApp = \case
  [_f, e@VNil] ->
    Just $ return e
  [f, VCons [x, xs]] -> Just $ do
    fx <- evalApp f [x]
    fxs <- case evalMapList evalApp [f, argExpr xs] of
      Nothing -> return $ VBuiltin (mkBuiltinFunction MapList) [Arg mempty Explicit Relevant f, xs]
      Just fxs -> fxs
    return $ VBuiltin (mkBuiltinConstructor Cons) (Arg mempty Explicit Relevant <$> [fx, fxs])
  _ -> Nothing

evalMapVector ::
  (MonadCompile m, PrintableBuiltin builtin) =>
  EvalApp builtin m ->
  EvalBuiltin builtin m
evalMapVector evalApp = \case
  [f, VVecLiteral xs] ->
    Just $ mkVLVec <$> traverse f' xs
    where
      f' x = evalApp f [x]
  _ -> Nothing

evalIndices :: EvalSimpleBuiltin builtin
evalIndices = \case
  [VNatLiteral n] -> Just $ mkVLVec (fmap VIndexLiteral [0 .. n - 1])
  _ -> Nothing

evalAnn :: EvalSimpleBuiltin builtin
evalAnn = \case
  [_t, e] -> Just e
  _ -> Nothing

-----------------------------------------------------------------------------
-- Derived

type EvalDerived builtin m =
  ( MonadCompile m,
    HasStandardData builtin
  ) =>
  [Value builtin] ->
  Maybe (m (Value builtin))

-- TODO define in terms of language. The problem is the polarity checking...
evalImplies :: EvalDerived builtin m
evalImplies = \case
  [e1, e2] -> Just $ do
    let defaultNot = VBuiltin (mkBuiltinFunction Not) [Arg mempty Explicit Relevant e1]
    let ne1 = fromMaybe defaultNot (evalNot [e1])
    let defaultOr = VBuiltin (mkBuiltinFunction Or) [Arg mempty Explicit Relevant ne1, Arg mempty Explicit Relevant e2]
    let maybeRes = evalOr [ne1, e2]
    return $ fromMaybe defaultOr maybeRes
  _ -> Nothing
