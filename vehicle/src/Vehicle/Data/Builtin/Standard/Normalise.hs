{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Normalise where

import Vehicle.Compile.Normalise.Builtin
import Vehicle.Compile.Normalise.Core
import Vehicle.Data.Builtin.Core as Syntax
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Prelude (HasIdentifier (identifierOf))

---------------------------------------------------------------------------------
--- Forced normalisation

instance NormalisableBuiltin Builtin where
  evalScheme = \case
    BuiltinFunction f -> case f of
      CompareIndex op -> Eval (evalCompareIndex op)
      CompareNat op -> Eval (evalCompareNat op)
      CompareRatTensor op -> Eval (evalCompareRatTensor op)
      Not -> Eval evalNot
      And -> Eval evalAnd
      Or -> Eval evalOr
      Add AddNat -> Eval evalAddNat
      Mul MulNat -> Eval evalMulNat
      Neg NegRatTensor -> Eval evalNegRatTensor
      Add AddRatTensor -> Eval evalAddRatTensor
      Sub SubRatTensor -> Eval evalSubRatTensor
      Mul MulRatTensor -> Eval evalMulRatTensor
      Div DivRatTensor -> Eval evalDivRatTensor
      Min MinRatTensor -> Eval evalMinRatTensor
      Max MaxRatTensor -> Eval evalMaxRatTensor
      Pow PowRatTensor -> Eval evalPowRatTensor
      Log LogRatTensor -> None
      Exp ExpRatTensor -> None
      ReduceAddRatTensor -> Eval evalReduceAddRatTensor
      ReduceMulRatTensor -> Eval evalReduceMulRatTensor
      ReduceMinRatTensor -> Eval evalReduceMinRatTensor
      ReduceMaxRatTensor -> Eval evalReduceMaxRatTensor
      ReduceAndTensor -> Eval evalReduceAndTensor
      ReduceOrTensor -> Eval evalReduceOrTensor
      If -> Eval evalIf
      Implies -> Eval evalImplies
      AtVector -> Eval evalAtVector
      AtTensor -> Eval evalAtTensor
      StackTensor -> Eval evalStackTensor
      ConstTensor -> Eval evalConstTensor
      FoldList -> Eval evalFoldList
      MapList -> Eval evalMapList
      ReverseList -> Eval evalReverseList
      AppendList -> Eval evalAppendList
      ForeachTensor -> Eval evalForeachTensor
      ForeachVector -> Eval evalForeachVector
      Iterate -> Eval evalIterate
      Transpose -> Eval evalTransposeTensor
      SearchRatTensor {} -> None
      WhereTensor {} -> None
      QuantifyRatTensor {} -> None
      QuantifyRecord {} -> None
    BuiltinCast c -> case c of
      FromNat FromNatToNat -> Eval forcedEvalFromNatToNat
      FromNat FromNatToIndex -> Eval forcedEvalFromNatToIndex
      FromNat FromNatToRat -> Eval forcedEvalFromNatToRat
      FromRat FromRatToRat -> Eval forcedEvalFromRatToRat
      FromVectorToList -> Eval forcedEvalVectorToList
    DerivedFunction f -> Derived (identifierOf f)
    TypeClassOp {} -> TypeClassOperation
    _ -> None

  isCast p b = case b of
    BuiltinCast c -> Just $ case c of
      FromNat FromNatToNat -> forceEvalSimpleBuiltin p b forcedEvalFromNatToNat
      FromNat FromNatToIndex -> forceEvalSimpleBuiltin p b forcedEvalFromNatToIndex
      FromNat FromNatToRat -> forceEvalSimpleBuiltin p b forcedEvalFromNatToRat
      FromRat FromRatToRat -> forceEvalSimpleBuiltin p b forcedEvalFromRatToRat
      FromVectorToList -> forceEvalSimpleBuiltin p b forcedEvalVectorToList
    BuiltinFunction StackTensor ->
      Just $
        -- Also force stacks to resolve as they are kind of cast.
        forceEvalSimpleBuiltin p b evalStackTensor
    _ -> Nothing

  isTypeClassOp = \case
    TypeClassOp {} -> True
    _ -> False

forcedEvalFromNatToNat ::
  (MonadNormBuiltin m) =>
  EvalSimple expr thunk FromNatToSimpleArgs Builtin m
forcedEvalFromNatToNat (FromNatToSimpleArgs v _) = return $ Evaluated v

forcedEvalFromNatToIndex ::
  forall m expr thunk.
  (MonadNormBuiltin m, HasBuiltinConstructor expr thunk) =>
  EvalSimple expr thunk FromNatToIndexArgs Builtin m
forcedEvalFromNatToIndex (FromNatToIndexArgs d value _) = do
  forcedValue <- force @expr value
  return $ case forcedValue of
    INatLiteral v -> Evaluated $ exprToThunk $ IIndexLiteral v d
    _ -> Unevaluable [forcedValue]

forcedEvalFromNatToRat ::
  forall m expr thunk.
  (MonadNormBuiltin m, HasBuiltinConstructor expr thunk) =>
  EvalSimple expr thunk FromNatToSimpleArgs Builtin m
forcedEvalFromNatToRat (FromNatToSimpleArgs value _) = do
  forcedValue <- force @expr value
  return $ case forcedValue of
    INatLiteral n -> Evaluated $ exprToThunk $ IRatLiteral $ Finite $ fromIntegral n
    _ -> Unevaluable [forcedValue]

forcedEvalFromRatToRat ::
  (MonadNormBuiltin m) =>
  EvalSimple expr thunk Op1Args Builtin m
forcedEvalFromRatToRat (Op1Args x) = return $ Evaluated x

forcedEvalVectorToList ::
  forall expr thunk m.
  (MonadNormBuiltin m, HasBuiltinConstructor expr thunk) =>
  EvalSimple expr thunk VectorToListArgs Builtin m
forcedEvalVectorToList (VectorToListArgs t d xs) = do
  d' <- force @expr d
  return $ case d' of
    INatLiteral n | n == length xs -> Evaluated $ exprToThunk $ mkListExpr t xs
    _ -> Unevaluable [d']

instance (HasBuiltinConstructor expr thunk) => HasTensorLiterals expr Builtin where
  tensorLiterals =
    [ Wrapper accessBoolTensorLiteral,
      Wrapper accessNatTensorLiteral,
      Wrapper accessRatTensorLiteral
    ]

instance (HasBuiltinConstructor expr thunk) => HasLiftableTensorOperations expr thunk Builtin where
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
    [ (accessCompareRatTensor, IRatType)
    ]
