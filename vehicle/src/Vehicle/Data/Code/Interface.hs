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
    getDim,
    getDims,
    getDimsExprs,
    mkIndexInto,
  )
where

import Control.Monad.Except (MonadError (..))
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface.Args as Args
import Vehicle.Data.Code.Interface.Operations as Operations
import Vehicle.Data.Code.Interface.Patterns as Patterns
import Vehicle.Data.Tensor

mkListExpr ::
  (HasListExpr expr builtin) =>
  expr builtin ->
  [expr builtin] ->
  expr builtin
mkListExpr tElem = foldr (ICons tElem) (INil tElem)

mkDims :: (HasNatExpr expr builtin, HasListExpr expr builtin, BuiltinHasNatType builtin) => [Int] -> expr builtin
mkDims ds = mkListExpr INatType (fmap INatLiteral ds)

getDim :: (HasNatExpr expr builtin) => expr builtin -> Maybe Int
getDim = \case
  INatLiteral n -> Just n
  _ -> Nothing

getDimsExprs :: (HasNatType expr builtin, HasNatExpr expr builtin, HasListExpr expr builtin) => expr builtin -> Either (expr builtin) [expr builtin]
getDimsExprs = \case
  IDimNil -> return []
  IDimCons d ds -> (d :) <$> getDimsExprs ds
  e -> throwError e

getDims :: (HasNatType expr builtin, HasNatExpr expr builtin, HasListExpr expr builtin) => expr builtin -> Maybe TensorShape
getDims v = case getDimsExprs v of
  Left {} -> Nothing
  Right xs -> traverse getDim xs

-- | Takes a `X` and [i_1, ... i_n] and returns `X ! i_1 ! i_n`
mkIndexInto ::
  forall expr builtin.
  (HasTensorExpr expr builtin) =>
  expr builtin ->
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
                    atFirstDim = INatLiteral d,
                    atRemainingDims = mkDims $ fmap fst xs,
                    atTensor = tensor,
                    atIndex = IIndexLiteral i (INatLiteral d)
                  }
        go result xs
