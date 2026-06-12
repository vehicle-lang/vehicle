module Vehicle.Data.Code.Interface.Patterns where

import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Code.Interface.Args
import Vehicle.Data.Code.Interface.Operations
import Vehicle.Data.Real (ExtendedRational)
import Vehicle.Data.Tensor

--------------------------------------------------------------------------------
-- Type patterns
--------------------------------------------------------------------------------

pattern IBoolType :: (HasBoolType expr builtin) => expr builtin
pattern IBoolType <- (getExpr accessBoolType -> Just ())
  where
    IBoolType = mkExpr accessBoolType ()

pattern IIndexType :: (HasIndexType expr builtin) => expr builtin -> expr builtin
pattern IIndexType size <- (getExpr accessIndexType -> Just (IndexTypeArgs size))
  where
    IIndexType size = mkExpr accessIndexType (IndexTypeArgs size)

pattern INatType :: (HasNatType expr builtin) => expr builtin
pattern INatType <- (getExpr accessNatType -> Just ())
  where
    INatType = mkExpr accessNatType ()

pattern IRatType :: (HasRatType expr builtin) => expr builtin
pattern IRatType <- (getExpr accessRatType -> Just ())
  where
    IRatType = mkExpr accessRatType ()

pattern IListType :: (HasListType expr builtin) => expr builtin -> expr builtin
pattern IListType tElem <- (getExpr accessListType -> Just (Op1Args tElem))
  where
    IListType tElem = mkExpr accessListType (Op1Args tElem)

pattern IVectorType :: (HasVectorType expr builtin) => expr builtin -> expr builtin -> expr builtin
pattern IVectorType tElem dims <- (getExpr accessVectorType -> Just (VectorTypeArgs tElem dims))
  where
    IVectorType tElem dims = mkExpr accessVectorType (VectorTypeArgs tElem dims)

pattern ITensorType :: (HasTensorType expr builtin) => expr builtin -> expr builtin -> expr builtin
pattern ITensorType tElem dims <- (getExpr accessTensorType -> Just (TensorTypeArgs tElem dims))
  where
    ITensorType tElem dims = mkExpr accessTensorType (TensorTypeArgs tElem dims)

--------------------------------------------------------------------------------
-- Literal patterns
--------------------------------------------------------------------------------

pattern IBoolTensorLiteral :: (HasBoolExpr expr builtin) => BoolTensor -> expr builtin
pattern IBoolTensorLiteral n <- (getExpr accessBoolTensorLiteral -> Just n)
  where
    IBoolTensorLiteral n = mkExpr accessBoolTensorLiteral n

pattern IBoolLiteral :: (HasBoolExpr expr builtin) => Bool -> expr builtin
pattern IBoolLiteral n = IBoolTensorLiteral (ZeroDimTensor n)

pattern IIndexLiteral :: (HasIndexExpr expr builtin) => Int -> expr builtin -> expr builtin
pattern IIndexLiteral n d <- (getExpr accessIndexLiteral -> Just (n, indexLiteralDim -> d))
  where
    IIndexLiteral n d = mkExpr accessIndexLiteral (n, IndexLiteralArgs d)

pattern INatLiteral :: (HasNatExpr expr builtin) => Int -> expr builtin
pattern INatLiteral n <- (getExpr accessNatLiteral -> Just n)
  where
    INatLiteral n = mkExpr accessNatLiteral n

pattern INatTensor :: (HasNatExpr expr builtin) => NatTensor -> expr builtin
pattern INatTensor n <- (getExpr accessNatTensorLiteral -> Just n)
  where
    INatTensor n = mkExpr accessNatTensorLiteral n

pattern IRatLiteral :: (HasRatExpr expr builtin) => ExtendedRational -> expr builtin
pattern IRatLiteral n = IRatTensor (ZeroDimTensor n)

pattern IRatTensor :: (HasRatExpr expr builtin) => ExtendedRatTensor -> expr builtin
pattern IRatTensor n <- (getExpr accessRatTensorLiteral -> Just n)
  where
    IRatTensor n = mkExpr accessRatTensorLiteral n

--------------------------------------------------------------------------------
-- Indices

pattern INil ::
  (HasListExpr expr builtin) =>
  expr builtin ->
  expr builtin
pattern INil t <- (getExpr accessNil -> Just (NilArgs t))
  where
    INil t = mkExpr accessNil (NilArgs t)

pattern ICons ::
  (HasListExpr expr builtin) =>
  expr builtin ->
  expr builtin ->
  expr builtin ->
  expr builtin
pattern ICons t x xs <- (getExpr accessCons -> Just (ConsArgs t x xs))
  where
    ICons t x xs = mkExpr accessCons (ConsArgs t x xs)

pattern IDimNil ::
  (HasListExpr expr builtin, HasNatType expr builtin) =>
  expr builtin
pattern IDimNil <- INil INatType
  where
    IDimNil = INil INatType

pattern IDimCons ::
  (HasListExpr expr builtin, HasNatType expr builtin) =>
  expr builtin ->
  expr builtin ->
  expr builtin
pattern IDimCons x xs <- ICons INatType x xs
  where
    IDimCons x xs = ICons INatType x xs

--------------------------------------------------------------------------------
-- Vector

pattern IVecLiteral :: (HasVectorExpr expr builtin) => expr builtin -> expr builtin -> [expr builtin] -> expr builtin
pattern IVecLiteral t d xs <- (getExpr accessVecLit -> Just (VectorLitArgs t d xs))
  where
    IVecLiteral t d xs = mkExpr accessVecLit (VectorLitArgs t d xs)
