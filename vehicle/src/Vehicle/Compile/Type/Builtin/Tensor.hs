module Vehicle.Compile.Type.Builtin.Tensor where

import Vehicle.Compile.Prelude
import Vehicle.Data.Builtin.Interface
import Vehicle.Data.Builtin.Tensor
import Vehicle.Data.Code.DSL
import Vehicle.Data.DSL
import Prelude hiding (pi)

-- See https://github.com/joelberkeley/spidr/blob/master/spidr/src/Tensor.idr

-- | Return the type of the provided builtin.
typeBuiltin :: Provenance -> TensorBuiltin -> Type TensorBuiltin
typeBuiltin p b = fromDSL p $ case b of
  TensorRat op -> typeRatTensorBuiltin op
  TensorBool op -> typeBoolTensorBuiltin op
  TensorDimType op -> typeDimensionType op
  TensorDimData op -> typeDimensionData op

typeDimensionType :: (BuiltinHasDimensionTypes builtin) => DimensionTypeBuiltin -> DSLExpr builtin
typeDimensionType = \case
  TensorType -> tDims ~> type0
  DimensionType {} -> type0
  DimensionsType {} -> type0
  DimensionIndexType -> tDim ~> type0

typeDimensionData :: (BuiltinHasDimensionTypes builtin, BuiltinHasDimensionData builtin) => DimensionDataBuiltin -> DSLExpr builtin
typeDimensionData = \case
  Dimension {} -> tDim
  DimensionNil -> tDims
  DimensionCons -> tDim ~> tDims ~> tDims
  -- No size checking yet
  DimensionIndex {} ->
    forAllDim $ \dim -> tDimIndex dim
  DimensionIndexTensor t ->
    forAllDim $ \dim -> tTensor (tDimIndex dim) (shapeOf t)
  ConstTensor {} ->
    forAll "A" type0 $ \tElem ->
      tElem ~> tDims ~> tTensor tElem tDims
  StackTensor n ->
    forAll "A" type0 $ \tElem ->
      forAllDims $ \dims ->
        let result = tTensor tElem (tCons (constDim n) dims)
         in iterate (\r -> tTensor tElem dims ~> r) result !! n
  DimensionLookup ->
    forAll "A" type0 $ \tElem ->
      forAllDim $ \dim ->
        forAllDims $ \dims ->
          tTensor tElem (tCons dim dims)
            ~> tDimIndex dim
            ~> tTensor tElem dims

typeRatTensorBuiltin :: (BuiltinHasDimensionTypes builtin, BuiltinHasDimensionData builtin, BuiltinHasRatTensor builtin) => RatTensorBuiltin -> DSLExpr builtin
typeRatTensorBuiltin = \case
  RatType -> type0
  RatTensor t -> tRatTensor (shapeOf t)
  RatLiteral {} -> tRatElemType
  NegRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims
  AddRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims ~> tRatTensor dims
  SubRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims ~> tRatTensor dims
  MulRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims ~> tRatTensor dims
  DivRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims ~> tRatTensor dims
  MinRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims ~> tRatTensor dims
  MaxRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims ~> tRatTensor dims
  ReduceAddRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor (tSingletonDim 1)
  ReduceMulRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor (tSingletonDim 1)
  ReduceMinRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor (tSingletonDim 1)
  ReduceMaxRatTensor -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor (tSingletonDim 1)
  SearchRatTensor {} ->
    forAllDims $ \dims ->
      -- Upper bounds for search space
      tRatTensor dims
        ~>
        -- Lower bounds for search space
        tRatTensor dims
        ~>
        -- Function to optimise for
        (tRatTensor dims ~> tRatTensor (tSingletonDim 1))
        ~>
        -- Function for combining search results
        forAllDim (\dim -> tRatTensor (tCons dim tNil) ~> tRatTensor (tSingletonDim 1))
        ~>
        -- Return type
        tRatTensor (tSingletonDim 1)

typeBoolTensorBuiltin :: (BuiltinHasDimensionTypes builtin, BuiltinHasDimensionData builtin, BuiltinHasRatTensor builtin, BuiltinHasBoolTensor builtin) => BoolTensorBuiltin -> DSLExpr builtin
typeBoolTensorBuiltin = \case
  BoolType -> type0
  BoolLiteral {} -> tBoolElemType
  BoolTensor t -> tBoolTensor (shapeOf t)
  NotBoolTensor -> forAllDims $ \dims -> tBoolTensor dims ~> tBoolTensor dims
  AndBoolTensor -> forAllDims $ \dims -> tBoolTensor dims ~> tBoolTensor dims ~> tBoolTensor dims
  OrBoolTensor -> forAllDims $ \dims -> tBoolTensor dims ~> tBoolTensor dims ~> tBoolTensor dims
  EqualsRatTensor {} -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims ~> tBoolTensor dims
  OrderRatTensor {} -> forAllDims $ \dims -> tRatTensor dims ~> tRatTensor dims ~> tBoolTensor dims
  ReduceAndTensor -> forAllDims $ \dims -> tBoolTensor dims ~> tBoolTensor (tSingletonDim 1)
  ReduceOrTensor -> forAllDims $ \dims -> tBoolTensor dims ~> tBoolTensor (tSingletonDim 1)
  QuantifyRatTensor {} ->
    forAllDims $ \dims ->
      (tRatTensor dims ~> tBoolTensor (tSingletonDim 1))
        ~> tBoolTensor (tSingletonDim 1)
