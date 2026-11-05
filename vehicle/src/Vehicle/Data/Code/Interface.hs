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

import Vehicle.Compile.Resource (AbstractNetworkModality (..), NetworkModality)
import Vehicle.Data.AST.Name (Name)
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.ForcedValue
import Vehicle.Data.Code.Interface.Args as Args
import Vehicle.Data.Code.Interface.Operations as Operations
import Vehicle.Data.Code.Interface.Patterns as Patterns
import Vehicle.Data.Tensor
import Vehicle.Data.Variable.Bound.Level (SlicePath (..), equivalentShapeOf)
import Vehicle.Prelude.Error (developerError)

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
  forall builtin.
  (HasTensorExpr ForcedValue Thunk builtin) =>
  Thunk builtin ->
  Thunk builtin ->
  NetworkModality TensorShape ->
  SlicePath ->
  Thunk builtin
mkIndexInto elementType value shapeOrShapes path = case shapeOrShapes of
  UniModal shape -> goUniModal value (equivalentShapeOf shape) path
  MultiModal _ shapes -> goMultiModal value (map (fmap equivalentShapeOf) shapes) path
  where
    goUniModal :: Thunk builtin -> TensorShape -> SlicePath -> Thunk builtin
    goUniModal tensor shape slice = case (shape, slice) of
      (d : ds, TensorPath i is) -> do
        let result =
              exprToThunk $
                mkExpr accessAtTensor $
                  AtTensorArgs
                    { atType = elementType,
                      atFirstDim = exprToThunk @ForcedValue $ INatLiteral d,
                      atRemainingDims = exprToThunk @ForcedValue (mkDims ds),
                      atTensor = tensor,
                      atIndex = exprToThunk @ForcedValue $ IIndexLiteral i (exprToThunk @ForcedValue $ INatLiteral d)
                    }
        goUniModal result ds is
      ([], EndPath) -> tensor
      _ -> developerError "Path does not match shape"

    goMultiModal :: Thunk builtin -> [(Name, TensorShape)] -> SlicePath -> Thunk builtin
    goMultiModal record fields slice = case (fields, slice) of
      ((currentField, currentShape) : rest, RecordPath fieldName indices)
        | currentField /= fieldName -> goMultiModal record rest slice
        | otherwise -> do
            let result = undefined
            goUniModal result currentShape indices
      _ -> developerError "Path does not match shape"
