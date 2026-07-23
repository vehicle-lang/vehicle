{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Normalise
  ( foldReduceAndComparison,
  )
where

import Vehicle.Data.Builtin.Core as Syntax
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Interface.Blocked
import Vehicle.Data.Builtin.Interface.Normalise
import Vehicle.Data.Builtin.Standard.Core
import Vehicle.Data.Code.Interface
import Vehicle.Data.Code.Value
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Prelude (GenericArg (..), HasIdentifier (identifierOf))

---------------------------------------------------------------------------------
--- Normalisation

instance (HasBuiltinConstructor expr) => HasTensorLiterals expr Builtin where
  tensorLiterals =
    [ Wrapper accessBoolTensorLiteral,
      Wrapper accessNatTensorLiteral,
      Wrapper accessRatTensorLiteral
    ]

instance HasLiftableTensorOperations Builtin where
  liftableTensorOp1s =
    [ (getExpr accessNegRatTensor, evalNegRatTensor, IRatType),
      (getExpr accessNotTensor, evalNot, IBoolType)
    ]

  liftableTensorOp2s =
    [ (getExpr accessAddRatTensor, evalAddRatTensor, IRatType),
      (getExpr accessMulRatTensor, evalMulRatTensor, IRatType),
      (getExpr accessSubRatTensor, evalSubRatTensor, IRatType),
      (getExpr accessDivRatTensor, evalDivRatTensor, IRatType),
      (getExpr accessMinRatTensor, evalMinRatTensor, IRatType),
      (getExpr accessMaxRatTensor, evalMaxRatTensor, IRatType),
      (getExpr accessAndTensor, evalAnd, IBoolType),
      (getExpr accessOrTensor, evalOr, IBoolType)
    ]

  liftableTensorComparisons =
    [ comparison Eq,
      comparison Ne,
      comparison Le,
      comparison Lt,
      comparison Ge
    ]
    where
      comparison op = (getExpr (accessArgsForOp accessCompareRatTensor op), evalCompareRatTensor op, IBoolType)

instance NormalisableBuiltin Builtin where
  evalScheme = \case
    BuiltinFunction f -> case f of
      CompareIndex op -> Simple (evalCompareIndex op)
      CompareNat op -> Simple (evalCompareNat op)
      CompareRatTensor op -> Simple (evalCompareRatTensor op)
      -- CompareRatTensorPointwise op -> Simple (evalCompareRatTensorPointwise op)
      Not -> Simple evalNot
      And -> Simple evalAnd
      Or -> Simple evalOr
      Add AddNat -> Simple evalAddNat
      Mul MulNat -> Simple evalMulNat
      Neg NegRatTensor -> Simple evalNegRatTensor
      Add AddRatTensor -> Simple evalAddRatTensor
      Sub SubRatTensor -> Simple evalSubRatTensor
      Mul MulRatTensor -> Simple evalMulRatTensor
      Div DivRatTensor -> Simple evalDivRatTensor
      Min MinRatTensor -> Simple evalMinRatTensor
      Max MaxRatTensor -> Simple evalMaxRatTensor
      Pow PowRatTensor -> Simple evalPowRatTensor
      Log LogRatTensor -> None
      Exp ExpRatTensor -> None
      ReduceAddRatTensor -> Simple evalReduceAddRatTensor
      ReduceMulRatTensor -> Simple evalReduceMulRatTensor
      ReduceMinRatTensor -> Simple evalReduceMinRatTensor
      ReduceMaxRatTensor -> Simple evalReduceMaxRatTensor
      ReduceAndTensor -> NonSimple evalReduceAndTensor
      ReduceOrTensor -> Simple evalReduceOrTensor
      If -> Simple evalIf
      Implies -> Simple evalImplies
      AtVector -> Simple evalAtVector
      AtTensor -> NonSimple evalAtTensor
      StackTensor -> Simple evalStackTensor
      ConstTensor -> Simple evalConstTensor
      FoldList -> NonSimple evalFoldList
      MapList -> NonSimple evalMapList
      ReverseList -> Simple evalReverseList
      AppendList -> Simple evalAppendList
      ForeachTensor -> NonSimple evalForeachTensor
      ForeachVector -> NonSimple evalForeachVector
      Iterate -> NonSimple evalIterate
      Transpose -> Simple evalTranspose
      QuantifyRatTensor {} -> None
      QuantifyRecord {} -> None
    BuiltinCast c -> case c of
      FromNat FromNatToNat -> Simple evalFromNatToNat
      FromNat FromNatToIndex -> Simple evalFromNatToIndex
      FromNat FromNatToRat -> Simple evalFromNatToRat
      FromRat FromRatToRat -> Simple evalFromRatToRat
      FromVectorToList -> Simple evalVectorToList
    DerivedFunction f -> Derived (identifierOf f)
    _ -> None

  blockingStatus = \case
    BuiltinFunction f -> functionBlockingStatus f
    BuiltinCast c -> castBlockingStatus c
    DerivedFunction f -> derivedFunctionBlockingStatus f
    _ -> return DoesNotReduce

  isTypeClassOp = \case
    TypeClassOp {} -> True
    _ -> False

  isCast p b = case b of
    BuiltinCast c -> Just $ case c of
      FromNat FromNatToNat -> forceEvalSimpleBuiltin p b evalFromNatToNat
      FromNat FromNatToIndex -> forceEvalSimpleBuiltin p b evalFromNatToIndex
      FromNat FromNatToRat -> forceEvalSimpleBuiltin p b evalFromNatToRat
      FromRat FromRatToRat -> forceEvalSimpleBuiltin p b evalFromRatToRat
      FromVectorToList -> forceEvalSimpleBuiltin p b evalVectorToList
    BuiltinFunction StackTensor ->
      Just $
        -- Also force stacks to resolve as they are kind of cast.
        forceEvalSimpleBuiltin p b evalStackTensor
    _ -> Nothing

evalFromNatToNat :: (MonadNormBuiltin m) => EvalSimple FromNatToSimpleArgs expr Builtin m
evalFromNatToNat (FromNatToSimpleArgs v _) = return v

evalFromNatToIndex :: (MonadNormBuiltin m, HasBuiltinConstructor expr) => EvalSimple FromNatToIndexArgs expr Builtin m
evalFromNatToIndex args = return $ case args of
  FromNatToIndexArgs d (INatLiteral v) _ -> IIndexLiteral v d
  _ -> mkExpr accessFromNatToIndex args

evalFromNatToRat :: (MonadNormBuiltin m, HasBuiltinConstructor expr) => EvalSimple FromNatToSimpleArgs expr Builtin m
evalFromNatToRat args = return $ case args of
  FromNatToSimpleArgs (INatLiteral n) _ -> IRatLiteral $ Finite $ fromIntegral n
  _ -> mkExpr accessFromNatToRat args

evalFromRatToRat :: (MonadNormBuiltin m) => EvalSimple Op1Args expr Builtin m
evalFromRatToRat (Op1Args x) = return x

evalVectorToList :: (MonadNormBuiltin m, HasBuiltinConstructor expr) => EvalSimple VectorToListArgs expr Builtin m
evalVectorToList args@(VectorToListArgs t d xs) =
  return $ case argExpr d of
    INatLiteral n | n == length xs -> mkListExpr (argExpr t) xs
    _ -> mkExpr accessFromVectorToList args

-- when going into the TensorReductionArgs expression, looking at what it is performing on
-- if it is a tensor comparison, then we can just reduce it
foldReduceAndComparison ::
  TensorReductionArgs (Value Builtin) ->
  Maybe (Value Builtin)
foldReduceAndComparison (TensorReductionArgs _ tensor) =
  case getExpr accessCompareRatTensor tensor of
    Just (op, TensorComparisonArgs dims IDimNil xs ys) | op /= Ne -> do
      let compareArgs = TensorComparisonArgs IDimNil dims xs ys
      Just $ mkExpr accessCompareRatTensor (op, compareArgs)
    _ -> Nothing
