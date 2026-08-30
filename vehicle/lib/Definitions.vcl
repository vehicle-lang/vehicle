--------------------------------------------------------------------------------
-- Type annotations
--------------------------------------------------------------------------------

-- Implementation of the `:` syntax
typeAnn : forallT (t : Type) . t -> t
typeAnn t a = a

--------------------------------------------------------------------------------
-- Bool
--------------------------------------------------------------------------------

forallInList : (A -> Bool) -> List A -> Bool
forallInList f xs = fold (\x y -> x and y) True (map f xs)

existsInList : (A -> Bool) -> List A -> Bool
existsInList f xs = fold (\x y -> x or y) False (map f xs)

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

existsIndex : forallT {n} . (Index n -> Bool) -> Bool
existsIndex f = reduceOr (foreach i . f i)

forallIndex : forallT {n} . (Index n -> Bool) -> Bool
forallIndex f = reduceAnd (foreach i . f i)

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

-- HasAdd
@typeclass
record HasAdd t1 t2 t3 where
  { addTC : t1 -> t2 -> t3
  }

@instance(default=0)
natHasAdd : HasAdd Nat Nat Nat
natHasAdd = { addTC = addNat }

@instance(default=1)
realTensorHasAdd : HasAdd (Tensor Real dims) (Tensor Real dims) (Tensor Real dims)
realTensorHasAdd = { addTC = addRealTensor }

-- HasSub
@typeclass
record HasSub t1 t2 t3 where
  { subTC : t1 -> t2 -> t3
  }

@instance
realTensorHasSub : HasSub (Tensor Real dims) (Tensor Real dims) (Tensor Real dims)
realTensorHasSub = { subTC = subRealTensor }

-- HasMul
@typeclass
record HasMul t1 t2 t3 where
  { mulTC : t1 -> t2 -> t3
  }

@instance(default=0)
natHasMul : HasMul Nat Nat Nat
natHasMul = { mulTC = mulNat }

@instance(default=1)
realTensorHasMul : HasMul (Tensor Real dims) (Tensor Real dims) (Tensor Real dims)
realTensorHasMul = { mulTC = mulRealTensor }

-- HasDiv
@typeclass
record HasDiv t1 t2 t3 where
  { divTC : t1 -> t2 -> t3
  }

@instance
realTensorHasDiv : HasDiv (Tensor Real dims) (Tensor Real dims) (Tensor Real dims)
realTensorHasDiv = { divTC = divRealTensor }

-- Quantifiers
@typeclass
record HasQuantifier t where
  { forallTC : (t -> Bool) -> Bool
  , existsTC : (t -> Bool) -> Bool
  }

@instance
indexHasQuantifier : HasQuantifier (Index n)
indexHasQuantifier =
  { forallTC = quantifyForAllIndex
  , existsTC = quantifyExistsIndex
  }

@instance
tensorHasQuantifier : HasQuantifier (Tensor Real ds)
tensorHasQuantifier =
  { forallTC = quantifyForallRealTensor
  , existsTC = quantifyExistsRealTensor
  }

-- Network IO
@typeclass
record HasValidNetworkIOType (t : Type) where {}

@instance
realTensorHasValidNetworkIOType : HasValidNetworkIOType (Tensor Real dims)
realTensorHasValidNetworkIOType = {}

-- Network Fields
@typeclass
record HasValidNetworkFieldType (t : Type) where {}

@instance
realTensorHasValidNetworkFieldType : HasValidNetworkFieldType (Tensor Real dims)
realTensorHasValidNetworkFieldType = {}

-- Network types
@typeclass
record HasValidNetworkType (t : Type) where {}

@instance
tensorToTensorHasValidNetworkType : {{ HasValidNetworkIOType t1 }} -> {{ HasValidNetworkIOType t2 }} -> HasValidNetworkType ( t1 -> t2 )
tensorToTensorHasValidNetworkType = {}

-- Comparisons
@typeclass
record HasComparison t1 t2 where
  { leTC : t1 -> t2 -> Bool
  , ltTC : t1 -> t2 -> Bool
  , geTC : t1 -> t2 -> Bool
  , gtTC : t1 -> t2 -> Bool
  , eqTC : t1 -> t2 -> Bool
  , neTC : t1 -> t2 -> Bool
  }

@instance
indexHasComparison : HasComparison (Index n1) (Index n2)
indexHasComparison =
  { leTC = compareIndexLe
  , ltTC = compareIndexLt
  , geTC = compareIndexGe
  , gtTC = compareIndexGt
  , eqTC = compareIndexEq
  , neTC = compareIndexNe
  }

@instance
natHasComparison : HasComparison Nat Nat
natHasComparison =
  { leTC = compareNatLe
  , ltTC = compareNatLt
  , geTC = compareNatGe
  , gtTC = compareNatGt
  , eqTC = compareNatEq
  , neTC = compareNatNe
  }

@instance
realTensorEmptyDimsHasComparison : HasComparison (Tensor Real []) (Tensor Real [])
realTensorEmptyDimsHasComparison =
  { leTC = compareRatTensorPointwiseLe
  , ltTC = compareRatTensorPointwiseLt
  , geTC = compareRatTensorPointwiseGe
  , gtTC = compareRatTensorPointwiseGt
  , eqTC = compareRatTensorPointwiseEq
  , neTC = compareRatTensorPointwiseNe
  }

@instance
realTensorHasComparison : HasComparison (Tensor Real (dim :: dims)) (Tensor Real (dim :: dims))
realTensorHasComparison =
  { leTC = compareRatTensorReducedLe
  , ltTC = compareRatTensorReducedLt
  , geTC = compareRatTensorReducedGe
  , gtTC = compareRatTensorReducedGt
  , eqTC = compareRatTensorReducedEq
  , neTC = compareRatTensorReducedNe
  }

-- Dataset tensor element types
@typeclass
record HasValidDatasetTensorElementType (t : Type) where {}

@instance
natHasValidDatasetTensorElementType : HasValidDatasetTensorElementType Nat
natHasValidDatasetTensorElementType = {}

@instance(default=0)
realHasValidDatasetTensorElementType : HasValidDatasetTensorElementType NonCastingReal
realHasValidDatasetTensorElementType = {}

-- Dataset list element types
@typeclass
record HasValidDatasetListElementType (t : Type) where {}

@instance
listHasValidDatasetListElementType : {{HasValidDatasetListElementType t}} -> HasValidDatasetListElementType (List t)
listHasValidDatasetListElementType = {}

@instance
vectorHasValidDatasetListElementType : {{HasValidDatasetListElementType t}} -> HasValidDatasetListElementType (Vector t dim)
vectorHasValidDatasetListElementType = {}

@instance
tensorHasValidDatasetListElementType : {{HasValidDatasetTensorElementType t}} -> HasValidDatasetListElementType (NonCastingTensor t dims)
tensorHasValidDatasetListElementType = {}

@instance
indexHasValidDatasetListElementType : HasValidDatasetListElementType (Index n)
indexHasValidDatasetListElementType = {}

@instance
natHasValidDatasetListElementType : HasValidDatasetListElementType Nat
natHasValidDatasetListElementType = {}

-- Dataset types
@typeclass
record HasValidDatasetType (t : Type) where {}

@instance
listHasValidDatasetType : {{HasValidDatasetListElementType t}} -> HasValidDatasetType (List t)
listHasValidDatasetType = {}

@instance
vectorHasValidDatasetType : {{HasValidDatasetListElementType t}} -> HasValidDatasetType (Vector t dim)
vectorHasValidDatasetType = {}

@instance
tensorHasValidDatasetType : {{HasValidDatasetTensorElementType t}} -> HasValidDatasetType (NonCastingTensor t dims)
tensorHasValidDatasetType = {}



--------------------------------------------------------------------------------
-- Loss logics
--------------------------------------------------------------------------------

{-
record DifferentiableElementLogic where
  { true             : Real
  , false            : Real
  , negation         : Real -> Real
  , conjunction      : Real -> Real -> Real
  , disjunction      : Real -> Real -> Real
  , lessThan         : Real -> Real -> Real
  , lessEqualThan    : Real -> Real -> Real
  , greaterThan      : Real -> Real -> Real
  , greaterEqualThan : Real -> Real -> Real
  , equal            : Real -> Real -> Real
  , notEqual         : Real -> Real -> Real
  }
-}

record DifferentiableTensorLogic where
  { trueElement               : Real
  , falseElement              : Real
  , pointwiseNegation         : Tensor Real dims -> Tensor Real dims
  , pointwiseConjunction      : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseDisjunction      : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseLessThan         : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseLessEqualThan    : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseGreaterThan      : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseGreaterEqualThan : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseEqual            : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseNotEqual         : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , reduceConjunction         : Tensor Real dims -> Real
  , reduceDisjunction         : Tensor Real dims -> Real
  }

VehicleLoss : DifferentiableTensorLogic
VehicleLoss =
  { trueElement                = -infinity
  , falseElement               = infinity
  , pointwiseNegation          = \x -> -x
  , pointwiseConjunction       = \x y -> max x y
  , pointwiseDisjunction       = \x y -> min x y
  , pointwiseLessThan          = \x y -> x - y
  , pointwiseLessEqualThan     = \x y -> x - y
  , pointwiseGreaterThan       = \x y -> y - x
  , pointwiseGreaterEqualThan  = \x y -> y - x
  , pointwiseEqual             = \x y -> min (x - y) (y - x)
  , pointwiseNotEqual          = \x y -> max (x - y) (y - x)
  , reduceConjunction          = \xs -> reduceMax xs
  , reduceDisjunction          = \xs -> reduceMin xs
  }

DL2Loss : DifferentiableTensorLogic
DL2Loss =
  { trueElement                = 0
  , falseElement               = infinity
  , pointwiseNegation          = \{dims} x -> (const 1 dims) / x
  , pointwiseConjunction       = \x y -> x + y
  , pointwiseDisjunction       = \x y -> x * y
  , pointwiseLessThan          = \{dims} x y -> max (const 0 dims) (x - y)
  , pointwiseLessEqualThan     = \{dims} x y -> max (const 0 dims) (x - y)
  , pointwiseGreaterThan       = \{dims} x y -> max (const 0 dims) (y - x)
  , pointwiseGreaterEqualThan  = \{dims} x y -> max (const 0 dims) (y - x)
  , pointwiseEqual             = \{dims} x y -> - (max (const 0 dims) (x - y) + max (const 0 dims) (y - x))
  , pointwiseNotEqual          = \{dims} x y -> (max (const 0 dims) (x - y) + max (const 0 dims) (y - x))
  , reduceConjunction          = \xs -> reduceAdd xs
  , reduceDisjunction          = \xs -> reduceMul xs
  }
