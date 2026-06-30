{-# OPTIONS_GHC -Wno-orphans #-}

module Vehicle.Data.Builtin.Standard.Normalise
  ( foldReduceAndComparison,
    evalTranspose,
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
import Vehicle.Data.Real (ExtendedRational (..))
import Vehicle.Data.Tensor (Tensor, TensorShape)
import Vehicle.Data.Tensor qualified as Tensor
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

evalTranspose :: (MonadNormBuiltin m) => EvalSimple TransposeArgs Value Builtin m
evalTranspose args@(TransposeArgs _ inputDims tensor) =
  return $
    fromMaybe (mkExpr accessTranspose args) $
      goLiteral <|> goConst <|> goStack
  where
    revDims :: Maybe (Value Builtin)
    revDims = mkDims . reverse <$> getDims inputDims

    goLiteral :: Maybe (Value Builtin)
    goLiteral =
      foldTensorLit accessNatTensorLiteral
        <|> foldTensorLit accessBoolTensorLiteral
        <|> foldTensorLit accessRatTensorLiteral

    foldTensorLit ::
      (Eq a) =>
      Accessor (Value Builtin) (Tensor a) ->
      Maybe (Value Builtin)
    foldTensorLit Access {getExpr = getLit, mkExpr = mkLit} = do
      t <- getLit tensor
      pure $ mkLit (Tensor.transposeTensor t)

    goConst :: Maybe (Value Builtin)
    goConst = do
      ConstTensorArgs t v _ <- getExpr accessConstTensor tensor
      rds <- revDims
      pure $ mkExpr accessConstTensor (ConstTensorArgs t v rds)

    goStack :: Maybe (Value Builtin)
    goStack = do
      shape <- getDims inputDims
      leaves <- gatherStack shape tensor
      pure $ buildStack tNat (reverse shape) (permuteFlat shape leaves)
      where
        tNat = INatType :: Value Builtin

    gatherStack :: TensorShape -> Value Builtin -> Maybe [Value Builtin]
    gatherStack [] v = Just [v]
    gatherStack (d : ds) v = do
      StackTensorArgs _ _ _ rows <- getExpr accessStackTensor v
      if length rows /= d
        then Nothing
        else concat <$> traverse (gatherStack ds) rows

    permuteFlat :: TensorShape -> [a] -> [a]
    permuteFlat shape leaves =
      [ leaves !! flattenIndices shape (reverse revIs)
        | revIs <- allMultiIndices (reverse shape)
      ]
      where
        flattenIndices ds is = sum (zipWith (*) is (drop 1 (scanr (*) 1 ds)))

    allMultiIndices :: TensorShape -> [[Int]]
    allMultiIndices = \case
      [] -> [[]]
      d : ds -> [i : rest | i <- [0 .. d - 1], rest <- allMultiIndices ds]

    buildStack :: Value Builtin -> TensorShape -> [Value Builtin] -> Value Builtin
    buildStack _tElem [] [v] = v
    buildStack _tElem [] _ = mkExpr accessTranspose args
    buildStack tElem (d : ds) vs =
      let chunkSize = product ds
          rows = chunksOf chunkSize vs
          subStacks = map (buildStack tElem ds) rows
       in mkExpr
            accessStackTensor
            ( StackTensorArgs
                tElem
                (INatLiteral d)
                (foldr (IDimCons . INatLiteral) IDimNil ds)
                subStacks
            )

    chunksOf :: Int -> [a] -> [[a]]
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)

foldReduceAndComparison ::
  TensorReductionArgs (Value Builtin) ->
  Maybe (Value Builtin)
foldReduceAndComparison (TensorReductionArgs _ tensor) =
  case getExpr accessCompareRatTensorPointwise tensor of
    (Just (op, TensorOp2Args (IDimCons d ds) xs ys)) | op /= Ne -> do
      let compareArgs = TensorReduceComparisonArgs d ds xs ys
      Just $ mkExpr accessCompareRatTensorReduced (op, compareArgs)
    _ -> Nothing
