-- | At various points in the compiler, we have different sets of builtins (e.g.
-- first time we type-check we use the standard set of builtins + type +
-- type classes, but when checking polarity and linearity information we
-- subsitute out all the types and type-classes for new types.)
--
-- The interfaces defined in this file allow us to abstract over the exact set
-- of builtins being used, and therefore allows us to define operations
-- (e.g. normalisation) once, rather than once for each builtin type.
module Vehicle.Data.Code.Interface
  ( module Args,
    module Operations,
    module Patterns,
    mkListExpr,
    mkDims,
    mkIndexInto,
  )
where

import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface.Args as Args
import Vehicle.Data.Code.Interface.Operations as Operations
import Vehicle.Data.Code.Interface.Patterns as Patterns
import Vehicle.Data.Tensor

mkListExpr ::
  (HasListExpr expr thunk builtin) =>
  thunk builtin ->
  [thunk builtin] ->
  expr builtin
mkListExpr tElem = foldr (\x xs -> ICons tElem x (exprToThunk xs)) (INil tElem)

mkDims :: forall expr thunk builtin. (HasNatType expr thunk builtin, HasNatExpr expr thunk builtin, HasListExpr expr thunk builtin) => [Int] -> expr builtin
mkDims ds = mkListExpr (exprToThunk @expr INatType) (fmap (exprToThunk @expr . INatLiteral) ds)

-- | Takes a `X` and [i_1, ... i_n] and returns `X ! i_1 ! i_n`
mkIndexInto ::
  forall expr thunk builtin.
  (HasTensorExpr expr thunk builtin) =>
  thunk builtin ->
  expr builtin ->
  TensorShape ->
  TensorIndices ->
  expr builtin
mkIndexInto elementType value shape indices = go value (zip shape indices)
  where
    go :: expr builtin -> [(TensorDimension, TensorIndex)] -> expr builtin
    go tensor = \case
      [] -> tensor
      (d, i) : xs -> do
        let result =
              mkExpr accessAtTensor $
                AtTensorArgs
                  { atType = elementType,
                    atFirstDim = exprToThunk @expr $ INatLiteral d,
                    atRemainingDims = exprToThunk @expr $ mkDims $ fmap fst xs,
                    atTensor = exprToThunk tensor,
                    atIndex = exprToThunk @expr $ IIndexLiteral i (exprToThunk @expr $ INatLiteral d)
                  }
        go result xs
