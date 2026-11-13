{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Vehicle.Data.Code.Interface.Args where

import Data.Hashable (Hashable)
import Data.Vector.Internal.Check (HasCallStack)
import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Interface
import Vehicle.Prelude

--------------------------------------------------------------------------------
-- Interface for arguments
--------------------------------------------------------------------------------

class IsArgs args where
  accessSpine :: Accessor [GenericArg expr] (args expr)

class HasLambdaConstructor expr thunk closure | expr -> thunk, thunk -> expr, thunk -> closure where
  accessForcedLamC :: Accessor (thunk builtin) (GenericBinder (thunk builtin), closure builtin)

--------------------------------------------------------------------------------
-- Op1Args

-- | Arguments for simple unary operations (`-` etc.)
newtype Op1Args expr = Op1Args
  { op1Arg :: expr
  }

traverseOp1Args :: (Applicative f) => (t1 -> f t2) -> Op1Args t1 -> f (Op1Args t2)
traverseOp1Args f (Op1Args xs) = Op1Args <$> f xs

instance IsArgs Op1Args where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [x]) -> Just $ Op1Args x
          _ -> Nothing,
        mkExpr = \(Op1Args x) -> explicit <$> [x]
      }

--------------------------------------------------------------------------------
-- Op2Args

-- | Arguments for simple binary operations (==, <= etc.)
data Op2Args expr = Op2Args
  { op2Arg1 :: expr,
    op2Arg2 :: expr
  }

traverseOp2Args :: (Applicative f) => (t1 -> f t2) -> Op2Args t1 -> f (Op2Args t2)
traverseOp2Args f (Op2Args xs ys) = Op2Args <$> f xs <*> f ys

instance IsArgs Op2Args where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [x, y]) -> Just $ Op2Args x y
          _ -> Nothing,
        mkExpr = \(Op2Args x y) -> explicit <$> [x, y]
      }

--------------------------------------------------------------------------------
-- VectorOp1Args

-- | Arguments for vector op operations
data VectorOp1Args expr = VectorOp1Args
  { vectorOp1Dim :: expr,
    vectorOp1Arg :: expr
  }

instance IsArgs VectorOp1Args where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [d, x]) -> Just $ VectorOp1Args d x
          _ -> Nothing,
        mkExpr = \(VectorOp1Args d x) -> [implicitIrrelevant d, explicit x]
      }

--------------------------------------------------------------------------------
-- TensorOp1Args

-- | Arguments for unary tensor operations (e.g. -, not)
data TensorOp1Args expr = TensorOp1Args
  { tensorOp1Dims :: expr,
    tensorOp1Arg :: expr
  }

instance IsArgs TensorOp1Args where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [ds, x]) -> Just $ TensorOp1Args ds x
          _ -> Nothing,
        mkExpr = \(TensorOp1Args ds x) -> [implicitIrrelevant ds, explicit x]
      }

--------------------------------------------------------------------------------
-- TensorOp2Args

-- | Arguments for binary tensor operations (e.g. +, -)
data TensorOp2Args expr = TensorOp2Args
  { tensorOp2Dims :: expr,
    tensorOp2Arg1 :: expr,
    tensorOp2Arg2 :: expr
  }

instance IsArgs TensorOp2Args where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [ds, x, y]) -> Just $ TensorOp2Args ds x y
          _ -> Nothing,
        mkExpr = \(TensorOp2Args ds x y) -> [implicitIrrelevant ds, explicit x, explicit y]
      }

traverseTensorOp2Args :: (Applicative f) => (t -> f t) -> TensorOp2Args t -> f (TensorOp2Args t)
traverseTensorOp2Args f (TensorOp2Args ds xs ys) = TensorOp2Args ds <$> f xs <*> f ys

mapTensorOp2Args :: (t -> t) -> TensorOp2Args t -> TensorOp2Args t
mapTensorOp2Args f (TensorOp2Args ds xs ys) = TensorOp2Args ds (f xs) (f ys)

--------------------------------------------------------------------------------
-- Tensor reduction args

-- | Arguments for tensor reduction operations (e.g. reduceAnd, reduceAdd)
data TensorReductionArgs expr = TensorReductionArgs
  { tensorReductionDims :: expr,
    tensorReductionTensor :: expr
  }

instance IsArgs TensorReductionArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [ds, xs]) -> Just $ TensorReductionArgs ds xs
          _ -> Nothing,
        mkExpr = \(TensorReductionArgs ds xs) -> [implicitIrrelevant ds, explicit xs]
      }

traverseReductionArgs :: (Applicative f) => (t -> f t) -> TensorReductionArgs t -> f (TensorReductionArgs t)
traverseReductionArgs f (TensorReductionArgs ds xs) = TensorReductionArgs ds <$> f xs

mapReductionArgs :: (t -> t) -> TensorReductionArgs t -> TensorReductionArgs t
mapReductionArgs f (TensorReductionArgs ds xs) = TensorReductionArgs ds (f xs)

--------------------------------------------------------------------------------
-- IndexComparisonArgs

-- | Arguments for comparisons (==, <= etc.) over Index
data IndexComparisonArgs expr = IndexComparisonArgs
  { indexCompSize1 :: expr,
    indexCompSize2 :: expr,
    indexCompArg1 :: expr,
    indexCompArg2 :: expr
  }

instance IsArgs IndexComparisonArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [n1, n2, x, y]) -> Just $ IndexComparisonArgs n1 n2 x y
          _ -> Nothing,
        mkExpr = \(IndexComparisonArgs n1 n2 x y) -> [implicitIrrelevant n1, implicitIrrelevant n2, explicit x, explicit y]
      }

-- | Arguments for comparisons (e.g. ==, <= etc. ) over tensors
data TensorComparisonArgs expr = TensorComparisonArgs
  { tensorPointwiseDims :: expr, -- implicit irrelevant, shape of tensor at the end
    tensorReduceDims :: expr, -- implicit relevant, dimensions reduction is performed on.
    tensorOp2Arg1 :: expr, -- explicit, tensor
    tensorOp2Arg2 :: expr -- explicit, tensor
  }

instance IsArgs TensorComparisonArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [dims1, dims2, x, y]) -> Just $ TensorComparisonArgs dims1 dims2 x y
          _ -> Nothing,
        mkExpr = \(TensorComparisonArgs dims1 dims2 x y) -> [implicitIrrelevant dims1, implicit dims2, explicit x, explicit y]
      }

-- -- | Arguments for binary tensor operations (e.g. +, -)
data TensorReduceComparisonArgs expr = TensorReduceComparisonArgs
  { tensorReduceOp2Dim :: expr,
    tensorReduceOp2Dims :: expr,
    tensorReduceOp2Arg1 :: expr,
    tensorReduceOp2Arg2 :: expr
  }

instance IsArgs TensorReduceComparisonArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [d, ds, x, y]) -> Just $ TensorReduceComparisonArgs d ds x y
          _ -> Nothing,
        mkExpr = \(TensorReduceComparisonArgs d ds x y) -> [implicitIrrelevant d, implicitIrrelevant ds, explicit x, explicit y]
      }

-- | Arguments for if
data IfArgs expr = IfArgs
  { ifType :: expr,
    ifCond :: expr,
    ifArg1 :: expr,
    ifArg2 :: expr
  }

instance IsArgs IfArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, c, x, y]) -> Just $ IfArgs t c x y
          _ -> Nothing,
        mkExpr = \(IfArgs t c x y) -> [implicit t, explicit c, explicit x, explicit y]
      }

traverseIfArgBranches :: (Applicative f) => (t -> f t) -> IfArgs t -> f (IfArgs t)
traverseIfArgBranches f (IfArgs t c x y) = IfArgs t c <$> f x <*> f y

mapIfArgBranches :: (t -> t) -> IfArgs t -> IfArgs t
mapIfArgBranches f (IfArgs t c x y) = IfArgs t c (f x) (f y)

--------------------------------------------------------------------------------
-- Vector
--------------------------------------------------------------------------------
-- VectorLitArgs

data VectorLitArgs expr = VectorLitArgs
  { vecLitType :: expr,
    vecLitDim :: expr,
    vecLitElements :: [expr]
  }

instance IsArgs VectorLitArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> t : d : xs) -> Just $ VectorLitArgs t d xs
          _ -> Nothing,
        mkExpr = \(VectorLitArgs t d xs) -> implicit t : implicitIrrelevant d : fmap explicit xs
      }

-- | Arguments for `!`
data AtVectorArgs expr = AtVectorArgs
  { atType :: expr,
    atDim :: expr,
    atVector :: expr,
    atIndex :: expr
  }

instance IsArgs AtVectorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, d, xs, i]) -> Just $ AtVectorArgs t d xs i
          _ -> Nothing,
        mkExpr = \(AtVectorArgs t d xs i) -> [implicit t, implicitIrrelevant d, explicit xs, explicit i]
      }

--------------------------------------------------------------------------------
-- Tensor
--------------------------------------------------------------------------------

-- | Arguments for `!`
data AtTensorArgs expr = AtTensorArgs
  { atType :: expr,
    atFirstDim :: expr,
    atRemainingDims :: expr,
    atTensor :: expr,
    atIndex :: expr
  }

instance IsArgs AtTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, d, ds, xs, i]) -> Just $ AtTensorArgs t d ds xs i
          _ -> Nothing,
        mkExpr = \(AtTensorArgs t d ds xs i) -> [implicit t, implicitIrrelevant d, implicitIrrelevant ds, explicit xs, explicit i]
      }

traverseAtTensorArg :: (Applicative f) => (t -> f t) -> AtTensorArgs t -> f (AtTensorArgs t)
traverseAtTensorArg f (AtTensorArgs t d ds tensor i) = AtTensorArgs t d ds <$> f tensor <*> pure i

mapAtTensorArg :: (t -> t) -> AtTensorArgs t -> AtTensorArgs t
mapAtTensorArg f (AtTensorArgs t d ds tensor i) = AtTensorArgs t d ds (f tensor) i

-- | Arguments for `ConstTensor`
data ConstTensorArgs expr = ConstTensorArgs
  { constType :: expr,
    constValue :: expr,
    constDims :: expr
  }

instance IsArgs ConstTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, v, ds]) -> Just $ ConstTensorArgs t v ds
          _ -> Nothing,
        mkExpr = \(ConstTensorArgs t v ds) -> [implicit t, explicit v, explicit ds]
      }

mapConstTensorValue :: (expr -> expr) -> ConstTensorArgs expr -> ConstTensorArgs expr
mapConstTensorValue f ConstTensorArgs {..} = ConstTensorArgs {constValue = f constValue, ..}

traverseConstTensorValue :: (Monad m) => (expr -> m expr) -> ConstTensorArgs expr -> m (ConstTensorArgs expr)
traverseConstTensorValue f ConstTensorArgs {..} = do
  constValue' <- f constValue
  return $ ConstTensorArgs {constValue = constValue', ..}

-- The trailing dims (@ds@) on 'StackTensorArgs', 'ForeachTensorArgs'
-- and 'TransposeTensorArgs' below are 'implicit' rather than
-- 'implicitIrrelevant': unification needs to solve the output shape
-- from the input shape (e.g. reversing @ds@ for 'Transpose'), which
-- requires the argument to participate in relevant elaboration.

-- | Arguments for `StackTensor`
data StackTensorArgs expr = StackTensorArgs
  { stackType :: expr,
    stackFirstDim :: expr,
    stackRemainingDims :: expr,
    stackElements :: [expr]
  }

instance IsArgs StackTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> t : d : ds : xs) -> Just $ StackTensorArgs t d ds xs
          _ -> Nothing,
        mkExpr = \(StackTensorArgs t d ds xs) -> implicit t : implicit d : implicit ds : fmap explicit xs
      }

mapStackTensorElements :: (expr -> expr) -> StackTensorArgs expr -> StackTensorArgs expr
mapStackTensorElements f StackTensorArgs {..} = StackTensorArgs {stackElements = fmap f stackElements, ..}

traverseStackTensorElements :: (Monad m) => (expr -> m expr) -> StackTensorArgs expr -> m (StackTensorArgs expr)
traverseStackTensorElements f StackTensorArgs {..} = do
  stackElements' <- traverse f stackElements
  return $ StackTensorArgs {stackElements = stackElements', ..}

-- | Arguments for `ForeachTensor`
data ForeachTensorArgs expr = ForeachTensorArgs
  { foreachTensorType :: expr,
    foreachTensorFirstDim :: expr,
    foreachTensorRemainingDims :: expr,
    foreachTensorFn :: expr
  }

instance IsArgs ForeachTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, d, ds, fn]) -> Just $ ForeachTensorArgs t d ds fn
          _ -> Nothing,
        mkExpr = \(ForeachTensorArgs t d ds fn) -> [implicit t, implicit d, implicit ds, explicit fn]
      }

-- | Arguments for `Transpose`
data TransposeTensorArgs expr = TransposeTensorArgs
  { transposeType :: expr,
    transposeDims :: expr,
    transposeTensor :: expr
  }

instance IsArgs TransposeTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, ds, xs]) -> Just $ TransposeTensorArgs t ds xs
          _ -> Nothing,
        mkExpr = \(TransposeTensorArgs t ds xs) -> [implicit t, implicit ds, explicit xs]
      }

traverseTransposeTensor :: (Applicative f) => (t -> f t) -> TransposeTensorArgs t -> f (TransposeTensorArgs t)
traverseTransposeTensor f (TransposeTensorArgs t ds xs) = TransposeTensorArgs t ds <$> f xs

-- | Arguments for `ForeachVector`
data ForeachVectorArgs expr = ForeachVectorArgs
  { foreachVectorType :: expr,
    foreachVectorDim :: expr,
    foreachVectorFn :: expr
  }

instance IsArgs ForeachVectorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, d, fn]) -> Just $ ForeachVectorArgs t d fn
          _ -> Nothing,
        mkExpr = \(ForeachVectorArgs t d fn) -> [implicit t, implicit d, explicit fn]
      }

-- | Arguments for `FromNat`
data FromNatToSimpleArgs expr = FromNatToSimpleArgs
  { fromNatArg :: expr,
    fromNatInDomain :: expr
  }

instance IsArgs FromNatToSimpleArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [x, d]) -> Just $ FromNatToSimpleArgs x d
          _ -> Nothing,
        mkExpr = \(FromNatToSimpleArgs x d) -> [explicit x, instanceIrrelevant d]
      }

-- | Arguments for `FromNatToIndex`
data FromNatToIndexArgs expr = FromNatToIndexArgs
  { indexSize :: expr,
    fromNatArg :: expr,
    fromNatInDomain :: GenericArg expr
  }

instance IsArgs FromNatToIndexArgs where
  accessSpine =
    Access
      { getExpr = \case
          [n, x, d] -> Just $ FromNatToIndexArgs (argExpr n) (argExpr x) d
          _ -> Nothing,
        mkExpr = \(FromNatToIndexArgs n x d) -> [implicitIrrelevant n, explicit x, d]
      }

--------------------------------------------------------------------------------
-- List
--------------------------------------------------------------------------------
-- Nil

-- | Arguments for `Nil`
newtype NilArgs expr = NilArgs
  { consType :: expr
  }

instance IsArgs NilArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t]) -> Just $ NilArgs t
          _ -> Nothing,
        mkExpr = \(NilArgs t) -> [implicit t]
      }

--------------------------------------------------------------------------------
-- Cons

-- | Arguments for `Cons`
data ConsArgs expr = ConsArgs
  { consType :: expr,
    consHead :: expr,
    consTails :: expr
  }

instance IsArgs ConsArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, x, xs]) -> Just $ ConsArgs t x xs
          _ -> Nothing,
        mkExpr = \(ConsArgs t x xs) -> [implicit t, explicit x, explicit xs]
      }

--------------------------------------------------------------------------------
-- MapList

-- | Arguments for `MapList`
data MapListArgs expr = MapListArgs
  { mapListInputType :: expr,
    mapListOutputType :: expr,
    mapListFun :: expr,
    mapListList :: expr
  }

instance IsArgs MapListArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t1, t2, fn, xs]) -> Just $ MapListArgs t1 t2 fn xs
          _ -> Nothing,
        mkExpr = \(MapListArgs t1 t2 fn xs) -> [implicit t1, implicit t2, explicit fn, explicit xs]
      }

--------------------------------------------------------------------------------
-- ReverseList

-- | Arguments for `ReverseList`
data ReverseListArgs expr = ReverseListArgs
  { reverseListElemType :: expr,
    reverseListList :: expr
  }

instance IsArgs ReverseListArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, xs]) -> Just $ ReverseListArgs t xs
          _ -> Nothing,
        mkExpr = \(ReverseListArgs t xs) -> [implicit t, explicit xs]
      }

--------------------------------------------------------------------------------
-- AppendList

-- | Arguments for `AppendList`
data AppendListArgs expr = AppendListArgs
  { appendListType :: expr,
    appendListOp1 :: expr,
    appendListOp2 :: expr
  }

instance IsArgs AppendListArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [t, xs, ys]) -> Just $ AppendListArgs t xs ys
          _ -> Nothing,
        mkExpr = \(AppendListArgs t xs ys) -> [implicit t, explicit xs, explicit ys]
      }

traverseAppendListArgs :: (Monad m) => (expr -> m expr) -> AppendListArgs expr -> m (AppendListArgs expr)
traverseAppendListArgs f AppendListArgs {..} = do
  appendListOp1' <- f appendListOp1
  appendListOp2' <- f appendListOp2
  return $ AppendListArgs {appendListOp1 = appendListOp1', appendListOp2 = appendListOp2', ..}

--------------------------------------------------------------------------------
-- FoldList

-- | Arguments for `MapList`
data FoldListArgs expr = FoldListArgs
  { foldListInputType :: GenericArg expr,
    foldListOutputType :: GenericArg expr,
    foldListFun :: expr,
    foldListDefault :: expr,
    foldListList :: expr
  }

instance IsArgs FoldListArgs where
  accessSpine =
    Access
      { getExpr = \case
          [t1, t2, fn, e, xs] -> Just $ FoldListArgs t1 t2 (argExpr fn) (argExpr e) (argExpr xs)
          _ -> Nothing,
        mkExpr = \(FoldListArgs t1 t2 fn e xs) -> [t1, t2, explicit fn, explicit e, explicit xs]
      }

--------------------------------------------------------------------------------
-- VectorToList

-- | Arguments for `VectorToList`
data VectorToListArgs expr = VectorToListArgs
  { vectorToListElementType :: expr,
    vectorToListSize :: expr,
    vectorToListArgs :: [expr]
  }

instance IsArgs VectorToListArgs where
  accessSpine =
    Access
      { getExpr = \case
          t : n : xs -> Just $ VectorToListArgs (argExpr t) (argExpr n) (fmap argExpr xs)
          _ -> Nothing,
        mkExpr = \(VectorToListArgs t n xs) -> implicit t : implicit n : fmap explicit xs
      }

-- | Arguments for `Iterate`
data IterateArgs expr = IterateArgs
  { iterateElementType :: GenericArg expr,
    iterateFn :: expr,
    iterateTimes :: expr,
    iterateStart :: expr
  }

instance IsArgs IterateArgs where
  accessSpine =
    Access
      { getExpr = \case
          [t, fn, n, e] -> Just $ IterateArgs t (argExpr fn) (argExpr n) (argExpr e)
          _ -> Nothing,
        mkExpr = \(IterateArgs t fn n e) -> [t, explicit fn, explicit n, explicit e]
      }

-- | Arguments for binary tensor operations (e.g. +, -)
newtype NetworkAppArgs expr = NetworkAppArgs
  { networkAppArg :: expr
  }
  deriving (Generic, Show, Eq)

instance (Hashable expr) => Hashable (NetworkAppArgs expr)

instance IsArgs NetworkAppArgs where
  accessSpine =
    Access
      { getExpr = \case
          [xs] -> Just $ NetworkAppArgs $ argExpr xs
          _ -> Nothing,
        mkExpr = \(NetworkAppArgs xs) -> [explicit xs]
      }

-- | Arguments for `QuantifyRatTensor`
-- Pointwise dims are not present/supplied in the frontend, and users don't directly interact with it.
-- They are instantiated to nil in the frontend, but will be used in the solver & loss backends.
data QuantifyRatTensorArgs expr body = QuantifyRatTensorArgs
  { quantifyPointwiseDims :: expr,
    quantifyBaseDims :: expr,
    quantifyBinder :: GenericBinder expr,
    quantifyBody :: body
  }

accessQuantifyRatTensorSpine ::
  (HasLambdaConstructor expr thunk closure) =>
  Accessor [GenericArg (thunk builtin)] (QuantifyRatTensorArgs (thunk builtin) (closure builtin))
accessQuantifyRatTensorSpine =
  Access
    { getExpr = \case
        (fmap argExpr -> [pDims, bDims, fn]) -> do
          let (binder, body) = accessLambda fn
          Just (QuantifyRatTensorArgs pDims bDims binder body)
        _ -> Nothing,
      mkExpr = \(QuantifyRatTensorArgs pDims bDims binder body) ->
        [ implicit pDims,
          implicitIrrelevant bDims,
          explicit (mkExpr accessForcedLamC (binder, body))
        ]
    }

accessLambda :: (HasCallStack, HasLambdaConstructor expr thunk closure) => thunk builtin -> (GenericBinder (thunk builtin), closure builtin)
accessLambda fn = case getExpr accessForcedLamC fn of
  Just (binder, body) -> (binder, body)
  Nothing -> developerError "expected search/quantifier function to be a lambda"

-- | Arguments for `QuantifyRecord`
data QuantifyRecordArgs expr body = QuantifyRecordArgs
  { quantifyRecordType :: expr,
    quantifyRecordBinder :: GenericBinder expr,
    quantifyRecordBody :: body
  }

accessQuantifyRecordSpine ::
  (HasLambdaConstructor expr thunk closure) =>
  Accessor [GenericArg (thunk builtin)] (QuantifyRecordArgs (thunk builtin) (closure builtin))
accessQuantifyRecordSpine =
  Access
    { getExpr = \case
        (fmap argExpr -> [typ, fn]) -> case getExpr accessForcedLamC fn of
          Just (binder, body) -> Just (QuantifyRecordArgs typ binder body)
          _ -> Nothing
        _ -> Nothing,
      mkExpr = \(QuantifyRecordArgs dims binder body) ->
        [ implicitIrrelevant dims,
          explicit (mkExpr accessForcedLamC (binder, body))
        ]
    }

--------------------------------------------------------------------------------
-- IndexTypeArgs

-- | Arguments for the `Index` type
newtype IndexTypeArgs expr = IndexTypeArgs
  { size :: expr
  }

instance IsArgs IndexTypeArgs where
  accessSpine =
    Access
      { getExpr = \case
          [x] -> Just $ IndexTypeArgs (argExpr x)
          _ -> Nothing,
        mkExpr = \(IndexTypeArgs x) -> [explicitIrrelevant x]
      }

--------------------------------------------------------------------------------
-- IndexTypeArgs

-- | Arguments for the `Index` type
newtype IndexLiteralArgs expr = IndexLiteralArgs
  { indexLiteralDim :: expr
  }

instance IsArgs IndexLiteralArgs where
  accessSpine =
    Access
      { getExpr = \case
          [d] -> Just $ IndexLiteralArgs (argExpr d)
          _ -> Nothing,
        mkExpr = \(IndexLiteralArgs d) -> [implicitIrrelevant d]
      }

--------------------------------------------------------------------------------
-- VectorTypeArgs

-- | Arguments for the `Index` type
data VectorTypeArgs expr = VectorTypeArgs
  { vectorElemType :: expr,
    vectorDim :: expr
  }

instance IsArgs VectorTypeArgs where
  accessSpine =
    Access
      { getExpr = \case
          [tElem, dim] -> Just $ VectorTypeArgs (argExpr tElem) (argExpr dim)
          _ -> Nothing,
        mkExpr = \(VectorTypeArgs tElem dim) -> [explicit tElem, explicitIrrelevant dim]
      }

--------------------------------------------------------------------------------
-- TensorTypeArgs

-- | Arguments for the `Index` type
data TensorTypeArgs expr = TensorTypeArgs
  { tensorElemType :: expr,
    tensorDims :: expr
  }

instance IsArgs TensorTypeArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [tElem, dims]) -> Just $ TensorTypeArgs tElem dims
          _ -> Nothing,
        mkExpr = \(TensorTypeArgs tElem dims) -> [explicit tElem, explicitIrrelevant dims]
      }

--------------------------------------------------------------------------------
-- SearchArgs

data SearchRatTensorArgs expr = SearchRatTensorArgs
  { searchDims :: expr,
    searchLowerBound :: expr,
    searchUpperBound :: expr,
    searchPredicate :: expr
  }

instance IsArgs SearchRatTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [dims, lower, upper, predicate]) ->
            Just $
              SearchRatTensorArgs
                { searchDims = dims,
                  searchLowerBound = lower,
                  searchUpperBound = upper,
                  searchPredicate = predicate
                }
          _ -> Nothing,
        mkExpr = \(SearchRatTensorArgs dims lower upper predicate) ->
          [ implicitIrrelevant dims,
            explicit lower,
            explicit upper,
            explicit predicate
          ]
      }

--------------------------------------------------------------------------------
-- WhereArgs

data WhereTensorArgs expr = WhereTensorArgs
  { whereDims :: expr,
    whereInput :: expr,
    whereCondition :: expr,
    whereValue :: expr
  }

instance IsArgs WhereTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [dims, input, cond, value]) ->
            Just $
              WhereTensorArgs
                { whereDims = dims,
                  whereInput = input,
                  whereCondition = cond,
                  whereValue = value
                }
          _ -> Nothing,
        mkExpr = \(WhereTensorArgs dims input cond value) ->
          [ implicitIrrelevant dims,
            explicit input,
            explicit cond,
            explicit value
          ]
      }

--------------------------------------------------------------------------------
-- FromBoolTensorToRatTensor (loss builtins)

data FromBoolTensorToRatTensorArgs expr = FromBoolTensorToRatTensorArgs
  { fromBoolTensorToRatTensorDims :: expr,
    fromBoolTensorToRatTensorTrue :: expr,
    fromBoolTensorToRatTensorFalse :: expr,
    fromBoolTensorToRatTensorTensor :: expr
  }

instance IsArgs FromBoolTensorToRatTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          (fmap argExpr -> [dims, trueValue, falseValue, tensor]) ->
            Just $
              FromBoolTensorToRatTensorArgs
                { fromBoolTensorToRatTensorDims = dims,
                  fromBoolTensorToRatTensorTrue = trueValue,
                  fromBoolTensorToRatTensorFalse = falseValue,
                  fromBoolTensorToRatTensorTensor = tensor
                }
          _ -> Nothing,
        mkExpr = \(FromBoolTensorToRatTensorArgs dims trueValue falseValue tensor) ->
          [ implicitIrrelevant dims,
            explicit trueValue,
            explicit falseValue,
            explicit tensor
          ]
      }
