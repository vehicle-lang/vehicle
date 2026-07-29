{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Normalise where

import Vehicle.Compile.Normalise.Builtin qualified as Forced
import Vehicle.Compile.Normalise.Core qualified as Forced
import Vehicle.Data.Builtin.Core as Syntax
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Prelude (HasIdentifier (identifierOf))

---------------------------------------------------------------------------------
--- Forced normalisation

instance Forced.NormalisableBuiltin Builtin where
  evalScheme = \case
    BuiltinFunction f -> case f of
      CompareIndex op -> Forced.Eval (Forced.evalCompareIndex op)
      CompareNat op -> Forced.Eval (Forced.evalCompareNat op)
      CompareRatTensor op -> Forced.Eval (Forced.evalCompareRatTensor op)
      Not -> Forced.Eval Forced.evalNot
      And -> Forced.Eval Forced.evalAnd
      Or -> Forced.Eval Forced.evalOr
      Add AddNat -> Forced.Eval Forced.evalAddNat
      Mul MulNat -> Forced.Eval Forced.evalMulNat
      Neg NegRatTensor -> Forced.Eval Forced.evalNegRatTensor
      Add AddRatTensor -> Forced.Eval Forced.evalAddRatTensor
      Sub SubRatTensor -> Forced.Eval Forced.evalSubRatTensor
      Mul MulRatTensor -> Forced.Eval Forced.evalMulRatTensor
      Div DivRatTensor -> Forced.Eval Forced.evalDivRatTensor
      Min MinRatTensor -> Forced.Eval Forced.evalMinRatTensor
      Max MaxRatTensor -> Forced.Eval Forced.evalMaxRatTensor
      Pow PowRatTensor -> Forced.Eval Forced.evalPowRatTensor
      Log LogRatTensor -> Forced.None
      Exp ExpRatTensor -> Forced.None
      ReduceAddRatTensor -> Forced.Eval Forced.evalReduceAddRatTensor
      ReduceMulRatTensor -> Forced.Eval Forced.evalReduceMulRatTensor
      ReduceMinRatTensor -> Forced.Eval Forced.evalReduceMinRatTensor
      ReduceMaxRatTensor -> Forced.Eval Forced.evalReduceMaxRatTensor
      ReduceAndTensor -> Forced.Eval Forced.evalReduceAndTensor
      ReduceOrTensor -> Forced.Eval Forced.evalReduceOrTensor
      If -> Forced.Eval Forced.evalIf
      Implies -> Forced.Eval Forced.evalImplies
      AtVector -> Forced.Eval Forced.evalAtVector
      AtTensor -> Forced.Eval Forced.evalAtTensor
      StackTensor -> Forced.Eval Forced.evalStackTensor
      ConstTensor -> Forced.Eval Forced.evalConstTensor
      FoldList -> Forced.Eval Forced.evalFoldList
      MapList -> Forced.Eval Forced.evalMapList
      AppendList -> Forced.Eval Forced.evalAppendList
      ForeachTensor -> Forced.Eval Forced.evalForeachTensor
      ForeachVector -> Forced.Eval Forced.evalForeachVector
      Iterate -> Forced.Eval Forced.evalIterate
      QuantifyRatTensor {} -> Forced.None
      QuantifyRecord {} -> Forced.None
    BuiltinCast c -> case c of
      FromNat FromNatToNat -> Forced.Eval forcedEvalFromNatToNat
      FromNat FromNatToIndex -> Forced.Eval forcedEvalFromNatToIndex
      FromNat FromNatToRat -> Forced.Eval forcedEvalFromNatToRat
      FromRat FromRatToRat -> Forced.Eval forcedEvalFromRatToRat
      FromVectorToList -> Forced.Eval forcedEvalVectorToList
    DerivedFunction f -> Forced.Derived (identifierOf f)
    TypeClassOp {} -> Forced.TypeClassOp
    _ -> Forced.None

  isCast p b = case b of
    BuiltinCast c -> Just $ case c of
      FromNat FromNatToNat -> Forced.forceEvalSimpleBuiltin p b forcedEvalFromNatToNat
      FromNat FromNatToIndex -> Forced.forceEvalSimpleBuiltin p b forcedEvalFromNatToIndex
      FromNat FromNatToRat -> Forced.forceEvalSimpleBuiltin p b forcedEvalFromNatToRat
      FromRat FromRatToRat -> Forced.forceEvalSimpleBuiltin p b forcedEvalFromRatToRat
      FromVectorToList -> Forced.forceEvalSimpleBuiltin p b forcedEvalVectorToList
    BuiltinFunction StackTensor ->
      Just $
        -- Also force stacks to resolve as they are kind of cast.
        Forced.forceEvalSimpleBuiltin p b Forced.evalStackTensor
    _ -> Nothing

  isTypeClassOp = \case
    TypeClassOp {} -> True
    _ -> False

forcedEvalFromNatToNat ::
  (Forced.MonadNormBuiltin m) =>
  Forced.EvalSimple expr thunk FromNatToSimpleArgs Builtin m
forcedEvalFromNatToNat (FromNatToSimpleArgs v _) = return $ Forced.Evaluated v

forcedEvalFromNatToIndex ::
  forall m expr thunk.
  (Forced.MonadNormBuiltin m, HasBuiltinConstructor expr thunk) =>
  Forced.EvalSimple expr thunk FromNatToIndexArgs Builtin m
forcedEvalFromNatToIndex (FromNatToIndexArgs d value _) = do
  forcedValue <- Forced.force @expr value
  return $ case forcedValue of
    INatLiteral v -> Forced.Evaluated $ exprToThunk $ IIndexLiteral v d
    _ -> Forced.Unevaluable [forcedValue]

forcedEvalFromNatToRat ::
  forall m expr thunk.
  (Forced.MonadNormBuiltin m, HasBuiltinConstructor expr thunk) =>
  Forced.EvalSimple expr thunk FromNatToSimpleArgs Builtin m
forcedEvalFromNatToRat (FromNatToSimpleArgs value _) = do
  forcedValue <- Forced.force @expr value
  return $ case forcedValue of
    INatLiteral n -> Forced.Evaluated $ exprToThunk $ IRatLiteral $ Finite $ fromIntegral n
    _ -> Forced.Unevaluable [forcedValue]

forcedEvalFromRatToRat ::
  (Forced.MonadNormBuiltin m) =>
  Forced.EvalSimple expr thunk Op1Args Builtin m
forcedEvalFromRatToRat (Op1Args x) = return $ Forced.Evaluated x

forcedEvalVectorToList ::
  forall expr thunk m.
  (Forced.MonadNormBuiltin m, HasBuiltinConstructor expr thunk) =>
  Forced.EvalSimple expr thunk VectorToListArgs Builtin m
forcedEvalVectorToList (VectorToListArgs t d xs) = do
  d' <- Forced.force @expr d
  return $ case d' of
    INatLiteral n | n == length xs -> Forced.Evaluated $ exprToThunk $ mkListExpr t xs
    _ -> Forced.Unevaluable [d']

instance (HasBuiltinConstructor expr thunk) => Forced.HasTensorLiterals expr Builtin where
  tensorLiterals =
    [ Forced.Wrapper accessBoolTensorLiteral,
      Forced.Wrapper accessNatTensorLiteral,
      Forced.Wrapper accessRatTensorLiteral
    ]

instance (HasBuiltinConstructor expr thunk) => Forced.HasLiftableTensorOperations expr thunk Builtin where
  liftableTensorOp1s =
    [ (accessNegRatTensor, IRatType),
      (accessNotTensor, IBoolType)
    ]

  liftableTensorOp2s =
    [ (accessAddRatTensor, IRatType),
      (accessMulRatTensor, IRatType),
      (accessSubRatTensor, IRatType),
      (accessDivRatTensor, IRatType),
      (accessMinRatTensor, IRatType),
      (accessMaxRatTensor, IRatType),
      (accessImpliesTensor, IBoolType),
      (accessAndTensor, IBoolType),
      (accessOrTensor, IBoolType)
    ]

  liftableTensorComparisons =
    [ comparison Eq,
      comparison Ne,
      comparison Le,
      comparison Lt,
      comparison Ge
    ]
    where
      comparison op = (accessArgsForOp accessCompareRatTensor op, IRatType)
