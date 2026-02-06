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
      (getExpr accessOrTensor, evalOr, IBoolType),
      compPointwise Eq,
      compPointwise Ne,
      compPointwise Le,
      compPointwise Lt,
      compPointwise Ge,
      compPointwise Gt
    ]
    where
      compPointwise op = (getExpr (accessArgsForOp accessCompareRatTensorPointwise op), evalCompareRatTensorPointwise op, IBoolType)

instance NormalisableBuiltin Builtin where
  evalScheme b = case b of
    BuiltinFunction f -> case f of
      CompareIndex op -> Simple (evalCompareIndex op)
      CompareNat op -> Simple (evalCompareNat op)
      CompareRatTensorPointwise op -> Simple (evalCompareRatTensorPointwise op)
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
      PowRat -> Simple evalPowRat
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
      ForeachTensor -> NonSimple evalForeachTensor
      ForeachVector -> NonSimple evalForeachVector
      Iterate -> NonSimple evalIterate
      QuantifyRatTensor {} -> None
      QuantifyTensorLike {} -> None
    BuiltinCast c -> case c of
      FromNat FromNatToNat -> Simple evalFromNatToNat
      FromNat FromNatToIndex -> Simple $ evalSimple b evalFromNatToIndex
      FromNat FromNatToRat -> Simple $ evalSimple b evalFromNatToRat
      FromRat FromRatToRat -> Simple evalFromRatToRat
      FromVectorToList -> Simple $ evalSimple b evalVectorToList
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

  evalCast b args = case b of
    BuiltinCast c -> case c of
      FromNat FromNatToNat -> forceEvalSimpleBuiltin (Just . evalFromNatToNat) args
      FromNat FromNatToIndex -> forceEvalSimpleBuiltin evalFromNatToIndex args
      FromNat FromNatToRat -> forceEvalSimpleBuiltin evalFromNatToRat args
      FromRat FromRatToRat -> forceEvalSimpleBuiltin (Just . evalFromRatToRat) args
      FromVectorToList -> forceEvalSimpleBuiltin evalVectorToList args
    BuiltinFunction StackTensor ->
      -- Also force stacks to resolve as they are kind of cast.
      forceEvalSimpleBuiltin partialEvalStackTensor args
    _ -> Nothing

evalFromNatToNat :: (MonadNormBuiltin m) => EvalSimple FromNatToSimpleArgs expr Builtin m
evalFromNatToNat (FromNatToSimpleArgs v _) = return v

evalFromNatToIndex :: (MonadNormBuiltin m, HasBuiltinConstructor expr) => EvalSimplePartial FromNatToIndexArgs expr Builtin m
evalFromNatToIndex args = case args of
  FromNatToIndexArgs _ (INatLiteral v) _ -> Just $ return $ IIndexLiteral v
  _ -> Nothing

-- mkExpr accessFromNatToIndex args

evalFromNatToRat :: (MonadNormBuiltin m, HasBuiltinConstructor expr) => EvalSimplePartial FromNatToSimpleArgs expr Builtin m
evalFromNatToRat args = case args of
  FromNatToSimpleArgs (INatLiteral n) _ -> Just $ return $ IRatLiteral $ fromIntegral n
  _ -> Nothing

evalFromRatToRat :: (MonadNormBuiltin m) => EvalSimple Op1Args expr Builtin m
evalFromRatToRat (Op1Args x) = return x

evalVectorToList :: (MonadNormBuiltin m, HasBuiltinConstructor expr) => EvalSimplePartial VectorToListArgs expr Builtin m
evalVectorToList (VectorToListArgs t d xs) =
  case argExpr d of
    INatLiteral n | n == length xs -> Just $ return $ mkListExpr (argExpr t) xs
    _ -> Nothing

foldReduceAndComparison ::
  TensorReductionArgs (Value Builtin) ->
  Maybe (Value Builtin)
foldReduceAndComparison (TensorReductionArgs _ _ tensor) =
  case getExpr accessCompareRatTensorPointwise tensor of
    Just (op, TensorOp2Args (IDimCons d ds) xs ys) -> do
      let compareArgs = TensorReduceComparisonArgs d ds xs ys
      Just $ mkExpr accessCompareRatTensorReduced (op, compareArgs)
    _ -> Nothing
