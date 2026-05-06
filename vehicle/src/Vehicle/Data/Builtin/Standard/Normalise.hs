{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Normalise
  ( foldReduceAndComparison,
  )
where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
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
  evalScheme = \case
    BuiltinFunction f -> case f of
      CompareIndex op -> Simple (evalCompareIndex op)
      CompareNat op -> Simple (evalCompareNat op)
      CompareRatTensorPointwise op -> Simple (evalCompareRatTensorPointwise op)
      Not -> Simple evalNot
      And -> Simple evalAnd
      Or -> Simple evalOr
      Temporal {} -> None
      Rollout -> None
      Add AddNat -> Simple evalAddNat
      Mul MulNat -> Simple evalMulNat
      Add AddTime -> Simple evalAddTime
      Sub SubTime -> Simple evalSubTime
      Mul MulTime -> Simple evalMulTime
      Div DivTime -> Simple evalDivTime
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
      Transpose -> Simple evalTranspose
      QuantifyRatTensor {} -> None
      QuantifyTensorLike {} -> None
    BuiltinCast c -> case c of
      FromNat FromNatToNat -> Simple evalFromNatToNat
      FromNat FromNatToIndex -> Simple evalFromNatToIndex
      FromNat FromNatToRat -> Simple evalFromNatToRat
      FromNat FromNatToTime -> Simple evalFromNatToTime
      FromTime FromTimeToNat -> Simple evalFromTimeToNat
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
      FromNat FromNatToTime -> forceEvalSimpleBuiltin p b evalFromNatToTime
      FromTime FromTimeToNat -> forceEvalSimpleBuiltin p b evalFromTimeToNat
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
  FromNatToSimpleArgs (INatLiteral n) _ -> IRatLiteral $ fromIntegral n
  _ -> mkExpr accessFromNatToRat args

evalFromNatToTime :: (MonadNormBuiltin m, HasBuiltinConstructor expr) => EvalSimple FromNatToSimpleArgs expr Builtin m
evalFromNatToTime args = return $ case args of
  FromNatToSimpleArgs (INatLiteral n) _ -> ITimeLiteral n
  _ -> mkExpr accessFromNatToTime args

evalFromTimeToNat :: (MonadNormBuiltin m, HasBuiltinConstructor expr) => EvalSimple Op1Args expr Builtin m
evalFromTimeToNat args = return $ case args of
  Op1Args (ITimeLiteral n) -> INatLiteral n
  _ -> mkExpr accessFromTimeToNat args

evalFromRatToRat :: (MonadNormBuiltin m) => EvalSimple Op1Args expr Builtin m
evalFromRatToRat (Op1Args x) = return x

evalVectorToList :: (MonadNormBuiltin m, HasBuiltinConstructor expr) => EvalSimple VectorToListArgs expr Builtin m
evalVectorToList args@(VectorToListArgs t d xs) =
  return $ case argExpr d of
    INatLiteral n | n == length xs -> mkListExpr (argExpr t) xs
    _ -> mkExpr accessFromVectorToList args

-- | Transpose normalisation. Fold concrete tensors; otherwise leave it for
-- index-through-transpose in `evalAtTensor`.
evalTranspose :: (MonadNormBuiltin m) => EvalSimple TransposeArgs Value Builtin m
evalTranspose args@(TransposeArgs _ resultDims tensor) =
  return $
    fromMaybe (mkExpr accessTranspose args) $
      -- ConstTensor is uniform: only dims change.
      goConst <|> goStack2D
  where
    goConst :: Maybe (Value Builtin)
    goConst = case getExpr accessConstTensor tensor of
      Just (ConstTensorArgs t v _) -> Just $ mkExpr accessConstTensor (ConstTensorArgs t v resultDims)
      Nothing -> Nothing

    -- 2-D Stack of Stacks: rebuild rows by swapping indices.
    goStack2D :: Maybe (Value Builtin)
    goStack2D = case getExpr accessStackTensor tensor of
      Just (StackTensorArgs t outerDim _ rows) -> do
        innerStacks <- traverse (getExpr accessStackTensor) rows
        case innerStacks of
          [] -> Nothing
          firstStack@(StackTensorArgs _ innerDim innerRest _) : _ ->
            -- Only handle 2-D for now: the inner stacks must have empty
            -- remaining dims.
            case innerRest of
              IDimNil -> do
                let innerCols = map stackElements innerStacks
                let n = length (stackElements firstStack)
                if any (\xs -> length xs /= n) innerCols
                  then Nothing
                  else do
                    let transposedRows =
                          [ mkExpr accessStackTensor (StackTensorArgs t outerDim IDimNil [row !! j | row <- innerCols])
                            | j <- [0 .. n - 1]
                          ]
                    Just $ mkExpr accessStackTensor (StackTensorArgs t innerDim (IDimCons outerDim IDimNil) transposedRows)
              _ -> Nothing
      Nothing -> Nothing

foldReduceAndComparison ::
  TensorReductionArgs (Value Builtin) ->
  Maybe (Value Builtin)
foldReduceAndComparison (TensorReductionArgs _ unit tensor) =
  case (unit, getExpr accessCompareRatTensorPointwise tensor) of
    (IBoolLiteral True, Just (op, TensorOp2Args (IDimCons d ds) xs ys)) | op /= Ne -> do
      let compareArgs = TensorReduceComparisonArgs d ds xs ys
      Just $ mkExpr accessCompareRatTensorReduced (op, compareArgs)
    _ -> Nothing
