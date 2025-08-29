module Vehicle.Data.Code.Interface where

import Control.Monad.Except (MonadError (..))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Tensor
import Vehicle.Prelude
import Vehicle.Syntax.Builtin.BasicOperations

--------------------------------------------------------------------------------
-- Interface to standard builtins
--------------------------------------------------------------------------------

class HasBuiltinConstructor expr where
  accessBuiltinC :: Accessor (expr builtin) (builtin, [GenericArg (expr builtin)])

mkBuiltin ::
  (HasBuiltinConstructor expr) =>
  Accessor builtin a ->
  a ->
  [GenericArg (expr builtin)] ->
  expr builtin
mkBuiltin accessBuiltin v args = mkExpr accessBuiltinC (mkExpr accessBuiltin v, args)

getBuiltin ::
  (HasBuiltinConstructor expr) =>
  Accessor builtin a ->
  expr builtin ->
  Maybe (a, [GenericArg (expr builtin)])
getBuiltin accessBuiltin e = case getExpr accessBuiltinC e of
  Just (b, args) -> case getExpr accessBuiltin b of
    Just v -> Just (v, args)
    _ -> Nothing
  _ -> Nothing

-- At various points in the compiler, we have different sets of builtins (e.g.
-- first time we type-check we use the standard set of builtins + type +
-- type classes, but when checking polarity and linearity information we
-- subsitute out all the types and type-classes for new types.)
--
-- The interfaces defined in this file allow us to abstract over the exact set
-- of builtins being used, and therefore allows us to define operations
-- (e.g. normalisation) once, rather than once for each builtin type.

class IsArgs args where
  accessSpine :: Accessor [GenericArg expr] (args expr)

-- | Arguments for simple unary operations (fromRatToRat etc.)
newtype Op1Args expr = Op1Args
  { op1Arg :: expr
  }

instance IsArgs Op1Args where
  accessSpine =
    Access
      { getExpr = \case
          [x] -> Just $ Op1Args (argExpr x)
          _ -> Nothing,
        mkExpr = \(Op1Args x) -> explicit <$> [x]
      }

-- | Arguments for simple binary operations (==, <= etc.)
data Op2Args expr = Op2Args
  { op2Arg1 :: expr,
    op2Arg2 :: expr
  }

instance IsArgs Op2Args where
  accessSpine =
    Access
      { getExpr = \case
          [x, y] -> Just $ Op2Args (argExpr x) (argExpr y)
          _ -> Nothing,
        mkExpr = \(Op2Args x y) -> explicit <$> [x, y]
      }

-- | Arguments for comparisons (==, <= etc.) over Index
data IndexComparisonArgs expr = IndexCompArgs
  { indexCompSize1 :: GenericArg expr,
    indexCompSize2 :: GenericArg expr,
    indexCompArg1 :: expr,
    indexCompArg2 :: expr
  }

instance IsArgs IndexComparisonArgs where
  accessSpine =
    Access
      { getExpr = \case
          [n1, n2, x, y] -> Just $ IndexCompArgs n1 n2 (argExpr x) (argExpr y)
          _ -> Nothing,
        mkExpr = \(IndexCompArgs n1 n2 x y) -> [n1, n2, explicit x, explicit y]
      }

-- | Arguments for vector op operations
data VectorOp1Args expr = VectorOp1Args
  { vectorOp1Dim :: GenericArg expr,
    vectorOp1Arg :: expr
  }

instance IsArgs VectorOp1Args where
  accessSpine =
    Access
      { getExpr = \case
          [d, x] -> Just $ VectorOp1Args d (argExpr x)
          _ -> Nothing,
        mkExpr = \(VectorOp1Args d x) -> [d, explicit x]
      }

-- | Arguments for unary tensor operations (e.g. -, not)
data TensorOp1Args expr = TensorOp1Args
  { tensorOp1Dims :: GenericArg expr,
    tensorOp1Arg :: expr
  }

instance IsArgs TensorOp1Args where
  accessSpine =
    Access
      { getExpr = \case
          [ds, x] -> Just $ TensorOp1Args ds (argExpr x)
          _ -> Nothing,
        mkExpr = \(TensorOp1Args ds x) -> [ds, explicit x]
      }

-- | Arguments for binary tensor operations (e.g. +, -)
data TensorOp2Args expr = TensorOp2Args
  { tensorOp2Dims :: GenericArg expr,
    tensorOp2Arg1 :: expr,
    tensorOp2Arg2 :: expr
  }

instance IsArgs TensorOp2Args where
  accessSpine =
    Access
      { getExpr = \case
          [ds, x, y] -> Just $ TensorOp2Args ds (argExpr x) (argExpr y)
          _ -> Nothing,
        mkExpr = \(TensorOp2Args ds x y) -> [ds, explicit x, explicit y]
      }

traverseTensorOp2Args :: (Applicative f) => (t -> f t) -> TensorOp2Args t -> f (TensorOp2Args t)
traverseTensorOp2Args f (TensorOp2Args ds xs ys) = TensorOp2Args ds <$> f xs <*> f ys

-- | Arguments for binary tensor operations (e.g. +, -)
data TensorReduceComparisonArgs expr = TensorReduceComparisonArgs
  { tensorReduceOp2Dim :: GenericArg expr,
    tensorReduceOp2Dims :: GenericArg expr,
    tensorReduceOp2Arg1 :: expr,
    tensorReduceOp2Arg2 :: expr
  }

instance IsArgs TensorReduceComparisonArgs where
  accessSpine =
    Access
      { getExpr = \case
          [d, ds, x, y] -> Just $ TensorReduceComparisonArgs d ds (argExpr x) (argExpr y)
          _ -> Nothing,
        mkExpr = \(TensorReduceComparisonArgs d ds x y) -> [d, ds, explicit x, explicit y]
      }

-- | Arguments for if
data IfArgs expr = IfArgs
  { ifType :: GenericArg expr,
    ifCond :: expr,
    ifArg1 :: expr,
    ifArg2 :: expr
  }

instance IsArgs IfArgs where
  accessSpine =
    Access
      { getExpr = \case
          [t, c, x, y] -> Just $ IfArgs t (argExpr c) (argExpr x) (argExpr y)
          _ -> Nothing,
        mkExpr = \(IfArgs t c x y) -> [t, explicit c, explicit x, explicit y]
      }

traverseIfArgBranches :: (Applicative f) => (t -> f t) -> IfArgs t -> f (IfArgs t)
traverseIfArgBranches f (IfArgs t c x y) = IfArgs t c <$> f x <*> f y

data VecLitArgs expr = VecLitArgs
  { vecLitType :: GenericArg expr,
    vecLitDim :: expr,
    vecLitElements :: [expr]
  }

instance IsArgs VecLitArgs where
  accessSpine =
    Access
      { getExpr = \case
          t : d : xs -> Just $ VecLitArgs t (argExpr d) (fmap argExpr xs)
          _ -> Nothing,
        mkExpr = \(VecLitArgs t d xs) -> t : implicitIrrelevant d : fmap explicit xs
      }

-- | Arguments for `!`
data AtVectorArgs expr = AtVectorArgs
  { atType :: GenericArg expr,
    atDim :: GenericArg expr,
    atVector :: expr,
    atIndex :: expr
  }

instance IsArgs AtVectorArgs where
  accessSpine =
    Access
      { getExpr = \case
          [t, d, xs, i] -> Just $ AtVectorArgs t d (argExpr xs) (argExpr i)
          _ -> Nothing,
        mkExpr = \(AtVectorArgs t d xs i) -> [t, d, explicit xs, explicit i]
      }

-- | Arguments for `!`
data AtTensorArgs expr = AtTensorArgs
  { atType :: GenericArg expr,
    atFirstDim :: GenericArg expr,
    atRemainingDims :: GenericArg expr,
    atTensor :: expr,
    atIndex :: expr
  }

instance IsArgs AtTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          [t, d, ds, xs, i] -> Just $ AtTensorArgs t d ds (argExpr xs) (argExpr i)
          _ -> Nothing,
        mkExpr = \(AtTensorArgs t d ds xs i) -> [t, d, ds, explicit xs, explicit i]
      }

traverseAtTensorArg :: (Applicative f) => (t -> f t) -> AtTensorArgs t -> f (AtTensorArgs t)
traverseAtTensorArg f (AtTensorArgs t d ds tensor i) = AtTensorArgs t d ds <$> f tensor <*> pure i

-- | Arguments for `ConstTensor`
data ConstTensorArgs expr = ConstTensorArgs
  { constType :: GenericArg expr,
    constValue :: expr,
    constDims :: expr
  }

instance IsArgs ConstTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          [t, v, ds] -> Just $ ConstTensorArgs t (argExpr v) (argExpr ds)
          _ -> Nothing,
        mkExpr = \(ConstTensorArgs t v ds) -> [t, explicit v, explicit ds]
      }

mapConstTensorValue :: (expr -> expr) -> ConstTensorArgs expr -> ConstTensorArgs expr
mapConstTensorValue f ConstTensorArgs {..} = ConstTensorArgs {constValue = f constValue, ..}

traverseConstTensorValue :: (Monad m) => (expr -> m expr) -> ConstTensorArgs expr -> m (ConstTensorArgs expr)
traverseConstTensorValue f ConstTensorArgs {..} = do
  constValue' <- f constValue
  return $ ConstTensorArgs {constValue = constValue', ..}

-- | Arguments for `StackTensor`
data StackTensorArgs expr = StackTensorArgs
  { stackType :: GenericArg expr,
    stackFirstDim :: expr,
    stackRemainingDims :: GenericArg expr,
    stackElements :: [expr]
  }

instance IsArgs StackTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          t : d : ds : xs -> Just $ StackTensorArgs t (argExpr d) ds (fmap argExpr xs)
          _ -> Nothing,
        mkExpr = \(StackTensorArgs t d ds xs) -> t : implicit d : ds : fmap explicit xs
      }

mapStackTensorElements :: (expr -> expr) -> StackTensorArgs expr -> StackTensorArgs expr
mapStackTensorElements f StackTensorArgs {..} = StackTensorArgs {stackElements = fmap f stackElements, ..}

traverseStackTensorElements :: (Monad m) => (expr -> m expr) -> StackTensorArgs expr -> m (StackTensorArgs expr)
traverseStackTensorElements f StackTensorArgs {..} = do
  stackElements' <- traverse f stackElements
  return $ StackTensorArgs {stackElements = stackElements', ..}

-- | Arguments for `ForeachTensor`
data ForeachTensorArgs expr = ForeachTensorArgs
  { foreachTensorType :: GenericArg expr,
    foreachTensorFirstDim :: expr,
    foreachTensorRemainingDims :: GenericArg expr,
    foreachTensorFn :: expr
  }

instance IsArgs ForeachTensorArgs where
  accessSpine =
    Access
      { getExpr = \case
          [t, d, ds, fn] -> Just $ ForeachTensorArgs t (argExpr d) ds (argExpr fn)
          _ -> Nothing,
        mkExpr = \(ForeachTensorArgs t d ds fn) -> [t, implicit d, ds, explicit fn]
      }

-- | Arguments for `ForeachVector`
data ForeachVectorArgs expr = ForeachVectorArgs
  { foreachVectorType :: GenericArg expr,
    foreachVectorDim :: expr,
    foreachVectorFn :: expr
  }

instance IsArgs ForeachVectorArgs where
  accessSpine =
    Access
      { getExpr = \case
          [t, d, fn] -> Just $ ForeachVectorArgs t (argExpr d) (argExpr fn)
          _ -> Nothing,
        mkExpr = \(ForeachVectorArgs t d fn) -> [t, implicit d, explicit fn]
      }

-- | Arguments for `FromNat`
data FromNatToSimpleArgs expr = FromNatToSimpleArgs
  { fromNatArg :: expr,
    fromNatInDomain :: GenericArg expr
  }

instance IsArgs FromNatToSimpleArgs where
  accessSpine =
    Access
      { getExpr = \case
          [x, d] -> Just $ FromNatToSimpleArgs (argExpr x) d
          _ -> Nothing,
        mkExpr = \(FromNatToSimpleArgs x d) -> [explicit x, d]
      }

-- | Arguments for `FromNatToIndex`
data FromNatToIndexArgs expr = FromNatToIndexArgs
  { indexSize :: GenericArg expr,
    fromNatArg :: expr,
    fromNatInDomain :: GenericArg expr
  }

instance IsArgs FromNatToIndexArgs where
  accessSpine =
    Access
      { getExpr = \case
          [n, x, d] -> Just $ FromNatToIndexArgs n (argExpr x) d
          _ -> Nothing,
        mkExpr = \(FromNatToIndexArgs n x d) -> [n, explicit x, d]
      }

-- | Arguments for `MapList`
data MapListArgs expr = MapListArgs
  { mapListInputType :: GenericArg expr,
    mapListOutputType :: GenericArg expr,
    mapListFun :: expr,
    mapListList :: expr
  }

instance IsArgs MapListArgs where
  accessSpine =
    Access
      { getExpr = \case
          [t1, t2, fn, xs] -> Just $ MapListArgs t1 t2 (argExpr fn) (argExpr xs)
          _ -> Nothing,
        mkExpr = \(MapListArgs t1 t2 fn xs) -> [t1, t2, explicit fn, explicit xs]
      }

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

-- | Arguments for `VectorToList`
data VectorToListArgs expr = VectorToListArgs
  { vectorToListElementType :: GenericArg expr,
    vectorToListSize :: GenericArg expr,
    vectorToListArgs :: [expr]
  }

instance IsArgs VectorToListArgs where
  accessSpine =
    Access
      { getExpr = \case
          t : n : xs -> Just $ VectorToListArgs t n (fmap argExpr xs)
          _ -> Nothing,
        mkExpr = \(VectorToListArgs t n xs) -> t : n : fmap explicit xs
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

type TensorReductionArgs = TensorOp2Args

type NatComparisonAccessor expr op = Accessor expr (op, Op2Args expr)

type IndexComparisonAccessor expr op = Accessor expr (op, IndexComparisonArgs expr)

type RatTensorPointwiseComparisonAccessor expr op = Accessor expr (op, TensorOp2Args expr)

type RatTensorReducedComparisonAccessor expr op = Accessor expr (op, TensorReduceComparisonArgs expr)

type Op1Accessor expr = Accessor expr expr

type Op2Accessor expr = Accessor expr (Op2Args expr)

type TensorOp1Accessor expr = Accessor expr (TensorOp1Args expr)

type TensorOp2Accessor expr = Accessor expr (TensorOp2Args expr)

type TensorReductionAccessor expr = Accessor expr (TensorReductionArgs expr)

accessNoArgs ::
  (HasBuiltinConstructor expr) =>
  Accessor builtin a ->
  Accessor (expr builtin) a
accessNoArgs access =
  Access
    { getExpr = \case
        (getBuiltin access -> Just (b, [])) -> Just b
        _ -> Nothing,
      mkExpr = \b -> mkBuiltin access b []
    }

accessArgs ::
  (HasBuiltinConstructor expr, IsArgs args) =>
  Accessor builtin () ->
  Accessor (expr builtin) (args (expr builtin))
accessArgs accessOp =
  Access
    { getExpr = \case
        (getBuiltin accessOp -> Just ((), getExpr accessSpine -> Just args)) -> Just args
        _ -> Nothing,
      mkExpr = \args -> mkBuiltin accessOp () (mkExpr accessSpine args)
    }

accessOpAndArgs ::
  (HasBuiltinConstructor expr, IsArgs args) =>
  Accessor builtin op ->
  Accessor (expr builtin) (op, args (expr builtin))
accessOpAndArgs accessOp =
  Access
    { getExpr = \case
        (getBuiltin accessOp -> Just (op, getExpr accessSpine -> Just args)) -> Just (op, args)
        _ -> Nothing,
      mkExpr = \(op, args) -> mkBuiltin accessOp op (mkExpr accessSpine args)
    }

accessArgsForOp ::
  (HasBuiltinConstructor expr, IsArgs args, Eq op) =>
  Accessor (expr builtin) (op, args (expr builtin)) ->
  op ->
  Accessor (expr builtin) (args (expr builtin))
accessArgsForOp accessor op =
  Access
    { getExpr = \case
        (getExpr accessor -> Just (op2, args)) | op == op2 -> Just args
        _ -> Nothing,
      mkExpr = \args -> mkExpr accessor (op, args)
    }

--------------------------------------------------------------------------------
-- Boolean operations
--------------------------------------------------------------------------------

type HasBoolExpr expr builtin =
  ( HasTensorExpr expr builtin,
    BuiltinHasBoolLiterals builtin
  )

accessBoolType :: (HasBuiltinConstructor expr, BuiltinHasBoolLiterals builtin) => Accessor (expr builtin) ()
accessBoolType = accessNoArgs accessBoolTypeBuiltin

accessBoolTensorLiteral :: (HasBoolExpr expr builtin) => Accessor (expr builtin) BoolTensor
accessBoolTensorLiteral = accessNoArgs accessBoolTensorLitBuiltin

accessNotTensor :: (HasBoolExpr expr builtin) => TensorOp1Accessor (expr builtin)
accessNotTensor = accessArgs accessNotBuiltin

accessAndTensor :: (HasBoolExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessAndTensor = accessArgs accessAndBuiltin

accessOrTensor :: (HasBoolExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessOrTensor = accessArgs accessOrBuiltin

accessImpliesTensor :: (HasBoolExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessImpliesTensor = accessArgs accessImpliesBuiltin

accessReduceAnd :: (HasBoolExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceAnd = accessArgs accessReduceAndBuiltin

accessReduceOr :: (HasBoolExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceOr = accessArgs accessReduceOrBuiltin

accessIf :: (HasBoolExpr expr builtin) => Accessor (expr builtin) (IfArgs (expr builtin))
accessIf = accessArgs accessIfBuiltin

accessCompareIndex :: (HasBoolExpr expr builtin) => IndexComparisonAccessor (expr builtin) ComparisonOp
accessCompareIndex = accessOpAndArgs accessCompareIndexBuiltin

accessCompareNat :: (HasBoolExpr expr builtin) => NatComparisonAccessor (expr builtin) ComparisonOp
accessCompareNat = accessOpAndArgs accessCompareNatBuiltin

accessCompareRatTensorPointwise :: (HasBoolExpr expr builtin) => RatTensorPointwiseComparisonAccessor (expr builtin) ComparisonOp
accessCompareRatTensorPointwise = accessOpAndArgs accessCompareRatTensorPointwiseBuiltin

accessCompareRatTensorReduced :: (HasBoolExpr expr builtin) => RatTensorReducedComparisonAccessor (expr builtin) ComparisonOp
accessCompareRatTensorReduced = accessOpAndArgs accessCompareRatTensorReducedBuiltin

accessQuantifyRatTensor :: (HasBoolExpr expr builtin) => Accessor (expr builtin) (Quantifier, GenericArg (expr builtin), expr builtin)
accessQuantifyRatTensor =
  Access
    { getExpr = \case
        (getBuiltin accessQuantifyRatTensorBuiltin -> Just (q, [ds, fn])) -> Just (q, ds, argExpr fn)
        _ -> Nothing,
      mkExpr = \(q, ds, fn) -> mkBuiltin accessQuantifyRatTensorBuiltin q [ds, explicit fn]
    }

pattern IBoolType :: (HasBuiltinConstructor expr, BuiltinHasBoolLiterals builtin) => expr builtin
pattern IBoolType <- (getExpr accessBoolType -> Just ())
  where
    IBoolType = mkExpr accessBoolType ()

pattern IBoolTensorLiteral :: (HasBoolExpr expr builtin) => BoolTensor -> expr builtin
pattern IBoolTensorLiteral n <- (getExpr accessBoolTensorLiteral -> Just n)
  where
    IBoolTensorLiteral n = mkExpr accessBoolTensorLiteral n

pattern IBoolLiteral :: (HasBoolExpr expr builtin) => Bool -> expr builtin
pattern IBoolLiteral n = IBoolTensorLiteral (ZeroDimTensor n)

--------------------------------------------------------------------------------
-- Indices

type HasIndexExpr expr builtin = (HasBuiltinConstructor expr, BuiltinHasIndexLiterals builtin)

accessIndexLiteral :: (HasIndexExpr expr builtin) => Accessor (expr builtin) Int
accessIndexLiteral = accessNoArgs accessIndexLitBuiltin

pattern IIndexLiteral :: (HasIndexExpr expr builtin) => Int -> expr builtin
pattern IIndexLiteral n <- (getExpr accessIndexLiteral -> Just n)
  where
    IIndexLiteral n = mkExpr accessIndexLiteral n

--------------------------------------------------------------------------------
-- Naturals

accessNatType :: (HasBuiltinConstructor expr, BuiltinHasNatType builtin) => Accessor (expr builtin) ()
accessNatType = accessNoArgs accessNatTypeBuiltin

type HasNatExpr expr builtin = (HasBuiltinConstructor expr, BuiltinHasNatLiterals builtin)

accessNatLiteral :: (HasNatExpr expr builtin) => Accessor (expr builtin) Int
accessNatLiteral = accessNoArgs accessNatLitBuiltin

accessNatTensorLiteral :: (HasNatExpr expr builtin) => Accessor (expr builtin) NatTensor
accessNatTensorLiteral = accessNoArgs accessNatTensorLitBuiltin

accessAddNat :: (HasNatExpr expr builtin) => Op2Accessor (expr builtin)
accessAddNat = accessArgs accessAddNatBuiltin

accessMulNat :: (HasNatExpr expr builtin) => Op2Accessor (expr builtin)
accessMulNat = accessArgs accessMulNatBuiltin

pattern INatType :: (HasBuiltinConstructor expr, BuiltinHasNatType builtin) => expr builtin
pattern INatType <- (getExpr accessNatType -> Just ())
  where
    INatType = mkExpr accessNatType ()

pattern INatLiteral :: (HasNatExpr expr builtin) => Int -> expr builtin
pattern INatLiteral n <- (getExpr accessNatLiteral -> Just n)
  where
    INatLiteral n = mkExpr accessNatLiteral n

pattern INatTensor :: (HasNatExpr expr builtin) => Tensor Int -> expr builtin
pattern INatTensor n <- (getExpr accessNatTensorLiteral -> Just n)
  where
    INatTensor n = mkExpr accessNatTensorLiteral n

--------------------------------------------------------------------------------
-- Rationals

type HasRatExpr expr builtin =
  ( HasTensorExpr expr builtin,
    BuiltinHasRatLiterals builtin
  )

accessRatType :: (HasBuiltinConstructor expr, BuiltinHasRatLiterals builtin) => Accessor (expr builtin) ()
accessRatType = accessNoArgs accessRatTypeBuiltin

accessRatTensorLiteral :: (HasRatExpr expr builtin) => Accessor (expr builtin) RatTensor
accessRatTensorLiteral = accessNoArgs accessRatTensorLitBuiltin

accessNegRatTensor :: (HasRatExpr expr builtin) => TensorOp1Accessor (expr builtin)
accessNegRatTensor = accessArgs accessNegRatTensorBuiltin

accessAddRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessAddRatTensor = accessArgs accessAddRatTensorBuiltin

accessMulRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessMulRatTensor = accessArgs accessMulRatTensorBuiltin

accessSubRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessSubRatTensor = accessArgs accessSubRatTensorBuiltin

accessDivRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessDivRatTensor = accessArgs accessDivRatTensorBuiltin

accessMinRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessMinRatTensor = accessArgs accessMinRatTensorBuiltin

accessMaxRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessMaxRatTensor = accessArgs accessMaxRatTensorBuiltin

accessPowRatTensor :: (HasRatExpr expr builtin) => TensorOp2Accessor (expr builtin)
accessPowRatTensor = accessArgs accessPowRatTensorBuiltin

accessReduceAddRat :: (HasRatExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceAddRat = accessArgs accessReduceAddRatBuiltin

accessReduceMulRat :: (HasRatExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceMulRat = accessArgs accessReduceMulRatBuiltin

accessReduceMinRat :: (HasRatExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceMinRat = accessArgs accessReduceMinRatBuiltin

accessReduceMaxRat :: (HasRatExpr expr builtin) => TensorReductionAccessor (expr builtin)
accessReduceMaxRat = accessArgs accessReduceMaxRatBuiltin

pattern IRatTensor :: (HasRatExpr expr builtin) => Tensor Rational -> expr builtin
pattern IRatTensor n <- (getExpr accessRatTensorLiteral -> Just n)
  where
    IRatTensor n = mkExpr accessRatTensorLiteral n

pattern IRatLiteral :: (HasRatExpr expr builtin) => Rational -> expr builtin
pattern IRatLiteral n = IRatTensor (ZeroDimTensor n)

pattern IRatType :: (HasBuiltinConstructor expr, BuiltinHasRatLiterals builtin) => expr builtin
pattern IRatType <- (getExpr accessRatType -> Just ())
  where
    IRatType = mkExpr accessRatType ()

--------------------------------------------------------------------------------
-- Lists

type HasListExpr expr builtin = (HasBuiltinConstructor expr, BuiltinHasListLiterals builtin)

accessNil ::
  (HasListExpr expr builtin) =>
  Accessor (expr builtin) (GenericArg (expr builtin))
accessNil = do
  Access
    { getExpr = \case
        (getBuiltin accessNilBuiltin -> Just ((), [t])) -> Just t
        _ -> Nothing,
      mkExpr = \t -> mkBuiltin accessNilBuiltin () [t]
    }

accessCons ::
  (HasListExpr expr builtin) =>
  Accessor (expr builtin) (GenericArg (expr builtin), expr builtin, expr builtin)
accessCons =
  Access
    { getExpr = \case
        (getBuiltin accessConsBuiltin -> Just ((), [t, x, xs])) -> Just (t, argExpr x, argExpr xs)
        _ -> Nothing,
      mkExpr = \(t, x, xs) -> mkBuiltin accessConsBuiltin () [t, explicit x, explicit xs]
    }

accessMapList :: (HasListExpr expr builtin) => Accessor (expr builtin) (MapListArgs (expr builtin))
accessMapList = accessArgs accessMapListBuiltin

accessFoldList :: (HasListExpr expr builtin) => Accessor (expr builtin) (FoldListArgs (expr builtin))
accessFoldList = accessArgs accessFoldListBuiltin

pattern INil ::
  (HasListExpr expr builtin) =>
  GenericArg (expr builtin) ->
  expr builtin
pattern INil t <- (getExpr accessNil -> Just t)
  where
    INil t = mkExpr accessNil t

pattern ICons ::
  (HasListExpr expr builtin) =>
  GenericArg (expr builtin) ->
  expr builtin ->
  expr builtin ->
  expr builtin
pattern ICons t x xs <- (getExpr accessCons -> Just (t, x, xs))
  where
    ICons t x xs = mkExpr accessCons (t, x, xs)

mkListExpr ::
  (HasListExpr expr builtin) =>
  expr builtin ->
  [expr builtin] ->
  expr builtin
mkListExpr tElem = foldr cons nil
  where
    nil = INil (implicit tElem)
    cons = ICons (implicit tElem)

mkDims :: (HasNatExpr expr builtin, HasListExpr expr builtin, BuiltinHasNatType builtin) => [Int] -> expr builtin
mkDims ds = mkListExpr INatType (fmap INatLiteral ds)

getDim :: (HasNatExpr expr builtin) => expr builtin -> Maybe Int
getDim = \case
  INatLiteral n -> Just n
  _ -> Nothing

getDimsExprs :: (HasNatExpr expr builtin, HasListExpr expr builtin) => expr builtin -> Either (expr builtin) [expr builtin]
getDimsExprs = \case
  INil _ -> return []
  ICons _ d ds -> (d :) <$> getDimsExprs ds
  e -> throwError e

getDims :: (HasNatExpr expr builtin, HasListExpr expr builtin) => expr builtin -> Maybe TensorShape
getDims v = case getDimsExprs v of
  Left {} -> Nothing
  Right xs -> traverse getDim xs

--------------------------------------------------------------------------------
-- Vector

type HasVectorExpr expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasVectors builtin,
    BuiltinHasNatLiterals builtin
  )

accessVecLit :: (HasVectorExpr expr builtin) => Accessor (expr builtin) (VecLitArgs (expr builtin))
accessVecLit = accessArgs accessVecLitBuiltin

accessAtVector :: (HasVectorExpr expr builtin) => Accessor (expr builtin) (AtVectorArgs (expr builtin))
accessAtVector = accessArgs accessAtVectorBuiltin

accessForeachVector ::
  (HasBuiltinConstructor expr, BuiltinHasForeach builtin) =>
  Accessor (expr builtin) (ForeachVectorArgs (expr builtin))
accessForeachVector = accessArgs accessForeachVectorBuiltin

pattern IVecLiteral :: (HasVectorExpr expr builtin) => GenericArg (expr builtin) -> expr builtin -> [expr builtin] -> expr builtin
pattern IVecLiteral t d xs <- (getExpr accessVecLit -> Just (VecLitArgs t d xs))
  where
    IVecLiteral t d xs = mkExpr accessVecLit (VecLitArgs t d xs)

--------------------------------------------------------------------------------
-- Tensors

type HasTensorExpr expr builtin =
  ( HasBuiltinConstructor expr,
    BuiltinHasTensors builtin,
    BuiltinHasListLiterals builtin,
    BuiltinHasNatLiterals builtin
  )

accessStackTensor :: (HasTensorExpr expr builtin) => Accessor (expr builtin) (StackTensorArgs (expr builtin))
accessStackTensor = accessArgs accessStackTensorBuiltin

accessConstTensor :: (HasTensorExpr expr builtin) => Accessor (expr builtin) (ConstTensorArgs (expr builtin))
accessConstTensor = accessArgs accessConstTensorBuiltin

accessAtTensor :: (HasTensorExpr expr builtin) => Accessor (expr builtin) (AtTensorArgs (expr builtin))
accessAtTensor = accessArgs accessAtTensorBuiltin

accessForeachTensor ::
  (HasBuiltinConstructor expr, BuiltinHasForeach builtin) =>
  Accessor (expr builtin) (ForeachTensorArgs (expr builtin))
accessForeachTensor = accessArgs accessForeachTensorBuiltin

accessIterate ::
  (HasBuiltinConstructor expr, BuiltinHasIterate builtin) =>
  Accessor (expr builtin) (IterateArgs (expr builtin))
accessIterate = accessArgs accessIterateBuiltin
