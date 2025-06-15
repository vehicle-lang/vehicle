module Vehicle.Data.Builtin.Interface.Blocked
  ( BlockingStatus (..),
    functionBlockingStatus,
    derivedFunctionBlockingStatus,
    castBlockingStatus,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Vehicle.Compile.Prelude (GenericArg (..))
import Vehicle.Data.Builtin.Interface (BuiltinHasNatLiterals)
import Vehicle.Data.Code.Interface (pattern INatLiteral)
import Vehicle.Data.Code.Value
import Vehicle.Syntax.Builtin

-----------------------------------------------------------------------------
-- Blocking arguments

type BlockingArgsTraversal builtin =
  forall m.
  (Monad m) =>
  (Value builtin -> m (Value builtin)) ->
  m (Spine builtin)

data BlockingStatus builtin
  = InsufficientArgs
  | DoesNotReduce
  | Blocked (BlockingArgsTraversal builtin)
  | AlwaysReduces

fixedStatus :: NonEmpty Int -> Spine builtin -> BlockingStatus builtin
fixedStatus indices spine
  | maximum indices < length spine = Blocked $ traverseArgsAtIndices (NonEmpty.toList indices) 0 spine
  | otherwise = InsufficientArgs

stackBlockingStatus :: (BuiltinHasNatLiterals builtin) => Spine builtin -> BlockingStatus builtin
stackBlockingStatus = \case
  [] -> InsufficientArgs
  (argExpr -> INatLiteral d) : xs
    | d == length xs -> AlwaysReduces
    | otherwise -> InsufficientArgs
  spine -> fixedStatus [0] spine

functionBlockingStatus ::
  (BuiltinHasNatLiterals builtin) =>
  BuiltinFunction ->
  Spine builtin ->
  BlockingStatus builtin
functionBlockingStatus b spine = case b of
  QuantifyRatTensor {} -> DoesNotReduce
  Implies -> AlwaysReduces
  Not -> fixedStatus [1] spine
  And -> fixedStatus [1, 2] spine
  Or -> fixedStatus [1, 2] spine
  Add AddNat -> fixedStatus [0, 1] spine
  Mul MulNat -> fixedStatus [0, 1] spine
  Neg NegRatTensor -> fixedStatus [1] spine
  Add AddRatTensor -> fixedStatus [1, 2] spine
  Mul MulRatTensor -> fixedStatus [1, 2] spine
  Sub SubRatTensor -> fixedStatus [1, 2] spine
  Div DivRatTensor -> fixedStatus [1, 2] spine
  Min MinRatTensor -> fixedStatus [1, 2] spine
  Max MaxRatTensor -> fixedStatus [1, 2] spine
  PowRat -> fixedStatus [0, 1] spine
  CompareIndex _op -> fixedStatus [2, 3] spine
  CompareNat _op -> fixedStatus [0, 1] spine
  CompareRatTensorPointwise _op -> fixedStatus [1, 2] spine
  If -> fixedStatus [1] spine
  AtTensor -> fixedStatus [3, 4] spine
  AtVector -> fixedStatus [2, 3] spine
  FoldList -> fixedStatus [4] spine
  MapList -> fixedStatus [3] spine
  ConstTensor -> fixedStatus [0, 1] spine
  ReduceAddRatTensor -> fixedStatus [1] spine
  ReduceMulRatTensor -> fixedStatus [1] spine
  ReduceMinRatTensor -> fixedStatus [1] spine
  ReduceMaxRatTensor -> fixedStatus [1] spine
  ReduceOrTensor -> fixedStatus [1] spine
  ReduceAndTensor -> fixedStatus [1] spine
  ForeachTensor -> fixedStatus [1] spine
  ForeachVector -> fixedStatus [1] spine
  Iterate -> fixedStatus [2] spine
  StackTensor -> stackBlockingStatus spine

derivedFunctionBlockingStatus :: DerivedFunction -> Spine builtin -> BlockingStatus builtin
derivedFunctionBlockingStatus f spine = case f of
  TypeAnn -> AlwaysReduces
  QuantifyIndex {} -> fixedStatus [0] spine
  QuantifyInList {} -> fixedStatus [2] spine
  CompareRatTensorReduced {} -> fixedStatus [1, 2] spine

castBlockingStatus :: BuiltinCast -> Spine builtin -> BlockingStatus builtin
castBlockingStatus f spine = case f of
  FromVectorToList -> fixedStatus [1] spine
  FromNat FromNatToIndex -> fixedStatus [1] spine
  FromNat FromNatToNat -> AlwaysReduces
  FromNat FromNatToRat -> fixedStatus [0] spine
  FromRat FromRatToRat -> AlwaysReduces

traverseArgsAtIndices ::
  (Monad m) =>
  [Int] ->
  Int ->
  Spine builtin ->
  (Value builtin -> m (Value builtin)) ->
  m (Spine builtin)
traverseArgsAtIndices _blockingArgs _currentIndex [] _f = return []
traverseArgsAtIndices [] _currentIndex args _f = return args
traverseArgsAtIndices (blockingIndex : blockingIndices) currentIndex (arg : args) f
  | currentIndex == blockingIndex = do
      arg' <- traverse f arg
      args' <- traverseArgsAtIndices blockingIndices (currentIndex + 1) args f
      return $ arg' : args'
  | otherwise = do
      args' <- traverseArgsAtIndices (blockingIndex : blockingIndices) (currentIndex + 1) args f
      return $ arg : args'
