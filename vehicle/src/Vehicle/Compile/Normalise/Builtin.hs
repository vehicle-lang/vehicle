{-# HLINT ignore "Use <|>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Vehicle.Compile.Normalise.Builtin where

import Control.Monad (zipWithM)
import Data.Foldable (foldrM)
import Data.Maybe (fromMaybe, mapMaybe)
import Vehicle.Compile.Error
import Vehicle.Compile.Prelude
import Vehicle.Compile.Print (PrintableBuiltin)
import Vehicle.Compile.Type.Meta (MetaSet)
import Vehicle.Compile.Type.Meta.Set qualified as MetaSet (unions)
import Vehicle.Expr.Normalisable
import Vehicle.Expr.Normalised

type EvalApp builtin m = Value builtin -> Spine builtin -> m (Value builtin)

type ForceArg builtin m = VArg builtin -> m (VArg builtin, (Bool, MetaSet))

class (PrintableBuiltin builtin) => Normalisable builtin where
  evalBuiltin ::
    (MonadCompile m, PrintableBuiltin builtin) =>
    EvalApp builtin m ->
    builtin ->
    Spine builtin ->
    m (Value builtin)

  isValue ::
    builtin ->
    Bool

  isTypeClassOp ::
    builtin ->
    Bool

  forceBuiltin ::
    (MonadCompile m, PrintableBuiltin builtin) =>
    EvalApp builtin m ->
    ForceArg builtin m ->
    builtin ->
    Spine builtin ->
    m (Maybe (Value builtin), MetaSet)

instance (Normalisable types) => Normalisable (NormalisableBuiltin types) where
  evalBuiltin evalApp b args = case b of
    CConstructor {} -> return $ VBuiltin b args
    CType {} -> return $ VBuiltin b args
    CFunction f -> do
      let result = evalBuiltinFunction evalApp f (mapMaybe getExplicitArg args)
      fromMaybe (return $ VBuiltin b args) result

  isValue = \case
    CConstructor {} -> True
    CType {} -> True
    CFunction {} -> False

  isTypeClassOp b = case b of
    CConstructor {} -> False
    CFunction {} -> False
    CType t -> isTypeClassOp t

  forceBuiltin evalApp forceArg b spine = case b of
    CConstructor {} -> return (Nothing, mempty)
    CType {} -> return (Nothing, mempty)
    CFunction {} -> do
      (argResults, argData) <- unzip <$> traverse forceArg spine
      let (argsReduced, argBlockingMetas) = unzip argData
      let anyArgsReduced = or argsReduced
      let blockingMetas = MetaSet.unions argBlockingMetas
      result <-
        if not anyArgsReduced
          then return Nothing
          else do
            Just <$> evalBuiltin evalApp b argResults
      return (result, blockingMetas)

-----------------------------------------------------------------------------
-- Indvidual builtins

evalBuiltinFunction ::
  (MonadCompile m, PrintableBuiltin (NormalisableBuiltin types)) =>
  EvalApp (NormalisableBuiltin types) m ->
  BuiltinFunction ->
  [Value (NormalisableBuiltin types)] ->
  Maybe (m (Value (NormalisableBuiltin types)))
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
      ConsVector -> return <$> evalConsVector args
      Fold dom -> evalFold dom evalApp args
      ZipWithVector -> evalZipWith evalApp args
      MapList -> evalMapList evalApp args
      MapVector -> evalMapVector evalApp args
      FromNat dom -> return <$> evalFromNat dom args
      FromRat dom -> return <$> evalFromRat dom args
      Indices -> return <$> evalIndices args
      Implies -> Just $ compilerDeveloperError $ "Found derived types" <+> pretty b
      Sample {} -> Just $ compilerDeveloperError $ "Should not be evaluating" <+> pretty b

isDerived :: BuiltinFunction -> Bool
isDerived = \case
  Implies {} -> True
  _ -> False

type EvalBuiltin types m =
  [Value (NormalisableBuiltin types)] ->
  Maybe (m (Value (NormalisableBuiltin types)))

type EvalSimpleBuiltin types =
  [Value (NormalisableBuiltin types)] ->
  Maybe (Value (NormalisableBuiltin types))

evalNot :: EvalSimpleBuiltin types
evalNot e = case e of
  [VBoolLiteral x] -> Just $ VBoolLiteral (not x)
  _ -> Nothing

evalAnd :: EvalSimpleBuiltin types
evalAnd = \case
  [VBoolLiteral x, VBoolLiteral y] -> Just $ VBoolLiteral (x && y)
  _ -> Nothing

evalOr :: EvalSimpleBuiltin types
evalOr = \case
  [VBoolLiteral x, VBoolLiteral y] -> Just $ VBoolLiteral (x || y)
  _ -> Nothing

evalNeg :: NegDomain -> EvalSimpleBuiltin types
evalNeg = \case
  NegInt -> evalNegInt
  NegRat -> evalNegRat

evalNegInt :: EvalSimpleBuiltin types
evalNegInt = \case
  [VIntLiteral x] -> Just $ VIntLiteral (-x)
  _ -> Nothing

evalNegRat :: EvalSimpleBuiltin types
evalNegRat = \case
  [VRatLiteral x] -> Just $ VRatLiteral (-x)
  _ -> Nothing

evalAdd :: AddDomain -> EvalSimpleBuiltin types
evalAdd = \case
  AddNat -> evalAddNat
  AddInt -> evalAddInt
  AddRat -> evalAddRat

evalAddNat :: EvalSimpleBuiltin types
evalAddNat = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VNatLiteral (x + y)
  _ -> Nothing

evalAddInt :: EvalSimpleBuiltin types
evalAddInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VIntLiteral (x + y)
  _ -> Nothing

evalAddRat :: EvalSimpleBuiltin types
evalAddRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x + y)
  _ -> Nothing

evalSub :: SubDomain -> EvalSimpleBuiltin types
evalSub = \case
  SubInt -> evalSubInt
  SubRat -> evalSubRat

evalSubInt :: EvalSimpleBuiltin types
evalSubInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VIntLiteral (x - y)
  _ -> Nothing

evalSubRat :: EvalSimpleBuiltin types
evalSubRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x - y)
  _ -> Nothing

evalMul :: MulDomain -> EvalSimpleBuiltin types
evalMul = \case
  MulNat -> evalMulNat
  MulInt -> evalMulInt
  MulRat -> evalMulRat

evalMulNat :: EvalSimpleBuiltin types
evalMulNat = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VNatLiteral (x * y)
  _ -> Nothing

evalMulInt :: EvalSimpleBuiltin types
evalMulInt = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VIntLiteral (x * y)
  _ -> Nothing

evalMulRat :: EvalSimpleBuiltin types
evalMulRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x * y)
  _ -> Nothing

evalDiv :: DivDomain -> EvalSimpleBuiltin types
evalDiv = \case
  DivRat -> evalDivRat

evalDivRat :: EvalSimpleBuiltin types
evalDivRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (x / y)
  _ -> Nothing

evalPowRat :: EvalSimpleBuiltin types
evalPowRat = \case
  [VRatLiteral x, VIntLiteral y] -> Just $ VRatLiteral (x ^^ y)
  _ -> Nothing

evalMinRat :: EvalSimpleBuiltin types
evalMinRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (min x y)
  _ -> Nothing

evalMaxRat :: EvalSimpleBuiltin types
evalMaxRat = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VRatLiteral (max x y)
  _ -> Nothing

evalOrder :: OrderDomain -> OrderOp -> EvalSimpleBuiltin types
evalOrder = \case
  OrderIndex -> evalOrderIndex
  OrderNat -> evalOrderNat
  OrderInt -> evalOrderInt
  OrderRat -> evalOrderRat

evalOrderIndex :: OrderOp -> EvalSimpleBuiltin types
evalOrderIndex op = \case
  [VIndexLiteral x, VIndexLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderNat :: OrderOp -> EvalSimpleBuiltin types
evalOrderNat op = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderInt :: OrderOp -> EvalSimpleBuiltin types
evalOrderInt op = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalOrderRat :: OrderOp -> EvalSimpleBuiltin types
evalOrderRat op = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VBoolLiteral (orderOp op x y)
  _ -> Nothing

evalEquals :: EqualityDomain -> EqualityOp -> EvalSimpleBuiltin types
evalEquals = \case
  EqIndex -> evalEqualityIndex
  EqNat -> evalEqualityNat
  EqInt -> evalEqualityInt
  EqRat -> evalEqualityRat

evalEqualityIndex :: EqualityOp -> EvalSimpleBuiltin types
evalEqualityIndex op = \case
  [VIndexLiteral x, VIndexLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityNat :: EqualityOp -> EvalSimpleBuiltin types
evalEqualityNat op = \case
  [VNatLiteral x, VNatLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityInt :: EqualityOp -> EvalSimpleBuiltin types
evalEqualityInt op = \case
  [VIntLiteral x, VIntLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalEqualityRat :: EqualityOp -> EvalSimpleBuiltin types
evalEqualityRat op = \case
  [VRatLiteral x, VRatLiteral y] -> Just $ VBoolLiteral (equalityOp op x y)
  _ -> Nothing

evalFromNat :: FromNatDomain -> EvalSimpleBuiltin types
evalFromNat = \case
  FromNatToIndex -> evalFromNatToIndex
  FromNatToNat -> evalFromNatToNat
  FromNatToInt -> evalFromNatToInt
  FromNatToRat -> evalFromNatToRat

evalFromNatToIndex :: EvalSimpleBuiltin types
evalFromNatToIndex = \case
  [VNatLiteral x] -> Just $ VIndexLiteral x
  _ -> Nothing

evalFromNatToNat :: EvalSimpleBuiltin types
evalFromNatToNat = \case
  [x] -> Just x
  _ -> Nothing

evalFromNatToInt :: EvalSimpleBuiltin types
evalFromNatToInt = \case
  [VNatLiteral x] -> Just $ VIntLiteral x
  _ -> Nothing

evalFromNatToRat :: EvalSimpleBuiltin types
evalFromNatToRat = \case
  [VNatLiteral x] -> Just $ VRatLiteral (fromIntegral x)
  _ -> Nothing

evalFromRat :: FromRatDomain -> EvalSimpleBuiltin types
evalFromRat = \case
  FromRatToRat -> evalFromRatToRat

evalFromRatToRat :: EvalSimpleBuiltin types
evalFromRatToRat = \case
  [x] -> Just x
  _ -> Nothing

evalIf :: EvalSimpleBuiltin types
evalIf = \case
  [VBoolLiteral True, e1, _e2] -> Just e1
  [VBoolLiteral False, _e1, e2] -> Just e2
  _ -> Nothing

evalAt :: EvalSimpleBuiltin types
evalAt = \case
  [VVecLiteral xs, VIndexLiteral i] -> Just $ case xs !!? fromIntegral i of
    Nothing -> developerError $ "out of bounds error:" <+> pretty (length xs) <+> "<=" <+> pretty i
    Just xsi -> argExpr xsi
  _ -> Nothing

evalConsVector :: EvalSimpleBuiltin types
evalConsVector = \case
  [x, VVecLiteral xs] -> Just $ mkVLVec (x : fmap argExpr xs)
  _ -> Nothing

evalFold ::
  (MonadCompile m, PrintableBuiltin (NormalisableBuiltin types)) =>
  FoldDomain ->
  EvalApp (NormalisableBuiltin types) m ->
  EvalBuiltin types m
evalFold = \case
  FoldList -> evalFoldList
  FoldVector -> evalFoldVector

evalFoldList ::
  (MonadCompile m, PrintableBuiltin (NormalisableBuiltin types)) =>
  EvalApp (NormalisableBuiltin types) m ->
  EvalBuiltin types m
evalFoldList evalApp = \case
  [_f, e, VNil] ->
    Just $ return e
  [f, e, VCons [x, xs]] ->
    Just $ do
      let defaultFold = return $ VBuiltinFunction (Fold FoldList) [RelevantExplicitArg mempty f, RelevantExplicitArg mempty e, xs]
      r <- fromMaybe defaultFold $ evalFoldList evalApp [f, e, argExpr xs]
      evalApp f [x, RelevantExplicitArg mempty r]
  _ -> Nothing

evalFoldVector ::
  (MonadCompile m, PrintableBuiltin (NormalisableBuiltin types)) =>
  EvalApp (NormalisableBuiltin types) m ->
  EvalBuiltin types m
evalFoldVector evalApp = \case
  [f, e, VVecLiteral xs] ->
    Just $ foldrM f' e xs
    where
      f' x r =
        evalApp
          f
          [ x,
            RelevantExplicitArg mempty r
          ]
  _ -> Nothing

evalZipWith ::
  (MonadCompile m, PrintableBuiltin (NormalisableBuiltin types)) =>
  EvalApp (NormalisableBuiltin types) m ->
  EvalBuiltin types m
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
  (MonadCompile m, PrintableBuiltin (NormalisableBuiltin types)) =>
  EvalApp (NormalisableBuiltin types) m ->
  EvalBuiltin types m
evalMapList evalApp = \case
  [_f, e@VNil] ->
    Just $ return e
  [f, VCons [x, xs]] -> Just $ do
    fx <- evalApp f [x]
    fxs <- case evalMapList evalApp [argExpr xs] of
      Nothing -> return $ VBuiltinFunction MapList [RelevantExplicitArg mempty f, xs]
      Just fxs -> fxs
    return $ VBuiltin (CConstructor Cons) (RelevantExplicitArg mempty <$> [fx, fxs])
  _ -> Nothing

evalMapVector ::
  (MonadCompile m, PrintableBuiltin (NormalisableBuiltin types)) =>
  EvalApp (NormalisableBuiltin types) m ->
  EvalBuiltin types m
evalMapVector evalApp = \case
  [f, VVecLiteral xs] ->
    Just $ mkVLVec <$> traverse f' xs
    where
      f' x = evalApp f [x]
  _ -> Nothing

evalIndices :: EvalSimpleBuiltin types
evalIndices = \case
  [VNatLiteral n] -> Just $ mkVLVec (fmap VIndexLiteral [0 .. n - 1])
  _ -> Nothing

-----------------------------------------------------------------------------
-- Derived

type EvalDerived types m =
  [Value (NormalisableBuiltin types)] ->
  Maybe (m (Value (NormalisableBuiltin types)))

-- TODO define in terms of language

evalImplies :: (MonadCompile m) => EvalDerived types m
evalImplies = \case
  [e1, e2] -> Just $ do
    let defaultNot = VBuiltinFunction Not [RelevantExplicitArg mempty e1]
    let ne1 = fromMaybe defaultNot (evalNot [e1])
    let defaultOr = VBuiltinFunction Or [RelevantExplicitArg mempty ne1, RelevantExplicitArg mempty e2]
    let maybeRes = evalOr [ne1, e2]
    return $ fromMaybe defaultOr maybeRes
  _ -> Nothing
