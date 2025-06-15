module Vehicle.Data.Builtin.Loss.Type where

typeRatSearchRatTensor :: (BuiltinHasDimensionTypes builtin, BuiltinHasDimensionData builtin, BuiltinHasRatTensor builtin) => RatTensorBuiltin -> DSLExpr builtin
typeRatSearchRatTensor = \case
  RatType -> type0
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
