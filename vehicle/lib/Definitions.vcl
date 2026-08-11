--------------------------------------------------------------------------------
-- Type annotations
--------------------------------------------------------------------------------

-- Implementation of the `:` syntax
typeAnn : forallT (t : Type) . t -> t
typeAnn t a = a

--------------------------------------------------------------------------------
-- Bool
--------------------------------------------------------------------------------

implies : Tensor Bool dims -> Tensor Bool dims -> Tensor Bool dims
implies x y = (not x) or y

forallInList : (A -> Bool) -> List A -> Bool
forallInList f xs = fold (\x y -> x and y) True (map f xs)

existsInList : (A -> Bool) -> List A -> Bool
existsInList f xs = fold (\x y -> x or y) False (map f xs)

--------------------------------------------------------------------------------
-- List
--------------------------------------------------------------------------------

-- List append. Cons-only definition via the right-fold primitive `fold`.
append : List A -> List A -> List A
append xs ys = fold (\x acc -> x :: acc) ys xs

-- Reverse a list. Implemented via fold and append; used in dimension lists
-- (e.g. inside `transpose`'s type signature). The expression-level form is
-- O(N^2) via repeated append, but dim lists are tiny so the cost is
-- irrelevant.
reverse : List A -> List A
reverse xs = fold (\x acc -> append acc (x :: [])) [] xs

--------------------------------------------------------------------------------
-- Tensor
--------------------------------------------------------------------------------
-- These operations have non-zero dimensions so that we have a unique
-- representation of relationships between zero-dimensional tensors
-- (i.e. pointwise comparison).

eqRatTensorReduced : Tensor Real (dim :: dims) -> Tensor Real (dim :: dims) -> Bool
eqRatTensorReduced xs ys = reduceAnd True (xs ==. ys)

neRatTensorReduced : Tensor Real (dim :: dims) -> Tensor Real (dim :: dims) -> Bool
neRatTensorReduced xs ys = not (eqRatTensorReduced xs ys)

leRatTensorReduced : Tensor Real (dim :: dims) -> Tensor Real (dim :: dims) -> Bool
leRatTensorReduced xs ys = reduceAnd True (xs <=. ys)

ltRatTensorReduced : Tensor Real (dim :: dims) -> Tensor Real (dim :: dims) -> Bool
ltRatTensorReduced xs ys = reduceAnd True (xs <. ys)

geRatTensorReduced : Tensor Real (dim :: dims) -> Tensor Real (dim :: dims) -> Bool
geRatTensorReduced xs ys = reduceAnd True (xs >=. ys)

gtRatTensorReduced : Tensor Real (dim :: dims) -> Tensor Real (dim :: dims) -> Bool
gtRatTensorReduced xs ys = reduceAnd True (xs >. ys)

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

existsIndex : forallT {n} . (Index n -> Bool) -> Bool
existsIndex f = reduceOr False (foreach i . f i)

forallIndex : forallT {n} . (Index n -> Bool) -> Bool
forallIndex f = reduceAnd True (foreach i . f i)

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

-- (HasAdd/HasSub/HasMul/HasDiv on `Time` live in STL.vcl)

-- HasSub
@typeclass
record HasSub t1 t2 t3 where
  { subTC : t1 -> t2 -> t3
  }

@instance(default=0)
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

@instance(default=0)
realTensorHasDiv : HasDiv (Tensor Real dims) (Tensor Real dims) (Tensor Real dims)
realTensorHasDiv = { divTC = divRealTensor }

-- HasPow: `x ** y`. Loss-backend only.
@typeclass
record HasPow t1 t2 t3 where
  { powTC : t1 -> t2 -> t3
  }

@instance
realTensorHasPow : HasPow (Tensor Real dims) (Tensor Real dims) (Tensor Real dims)
realTensorHasPow = { powTC = powRealTensor }

-- HasExp: `exp x`. Loss-backend only.
@typeclass
record HasExp t where
  { expTC : t -> t
  }

@instance
realTensorHasExp : HasExp (Tensor Real dims)
realTensorHasExp = { expTC = expRealTensor }

-- HasLog: `log b x` (logarithm of `x` with base `b`). Loss-backend only.
@typeclass
record HasLog t1 t2 t3 where
  { logTC : t1 -> t2 -> t3
  }

@instance
realTensorHasLog : HasLog (Tensor Real dims) (Tensor Real dims) (Tensor Real dims)
realTensorHasLog = { logTC = logRealTensor }

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

-- Dynamics types (two-input functions: State -> Action -> State)
@typeclass
record HasValidDynamicsType (t : Type) where {}

@instance
tensorToTensorToTensorHasValidDynamicsType : {{ HasValidNetworkIOType t1 }} -> {{ HasValidNetworkIOType t2 }} -> {{ HasValidNetworkIOType t3 }} -> HasValidDynamicsType ( t1 -> t2 -> t3 )
tensorToTensorToTensorHasValidDynamicsType = {}

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
indexHasComparison =  { leTC = compareIndexLe
                      , ltTC = compareIndexLt
                      , geTC = compareIndexGe
                      , gtTC = compareIndexGt
                      , eqTC = compareIndexEq
                      , neTC = compareIndexNe
                      }

@instance
natHasComparison : HasComparison Nat Nat
natHasComparison =  { leTC = compareNatLe
                    , ltTC = compareNatLt
                    , geTC = compareNatGe
                    , gtTC = compareNatGt
                    , eqTC = compareNatEq
                    , neTC = compareNatNe
                    }

@instance
realTensorEmptyDimsHasComparison : HasComparison (Tensor Real []) (Tensor Real [])
realTensorEmptyDimsHasComparison = { leTC = compareRatTensorPointwiseLe
                                   , ltTC = compareRatTensorPointwiseLt
                                   , geTC = compareRatTensorPointwiseGe
                                   , gtTC = compareRatTensorPointwiseGt
                                   , eqTC = compareRatTensorPointwiseEq
                                   , neTC = compareRatTensorPointwiseNe
                                   }

@instance
realTensorHasComparison : HasComparison (Tensor Real (dim :: dims)) (Tensor Real (dim :: dims))
realTensorHasComparison = { leTC = compareRatTensorReducedLe
                          , ltTC = compareRatTensorReducedLt
                          , geTC = compareRatTensorReducedGe
                          , gtTC = compareRatTensorReducedGt
                          , eqTC = compareRatTensorReducedEq
                          , neTC = compareRatTensorReducedNe
                          }

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
  -- Temporal operators (Globally, Finally, Until) lift `pointwiseConjunction`
  -- and `pointwiseDisjunction` as time-indexed reductions, with `trueElement`
  -- and `falseElement` as the reduction identities (standard STL derivation).
  , pointwiseLessThan         : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseLessEqualThan    : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseGreaterThan      : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseGreaterEqualThan : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseEqual            : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , pointwiseNotEqual         : Tensor Real dims -> Tensor Real dims -> Tensor Real dims
  , reduceConjunction         : Real -> Tensor Real dims -> Real
  , reduceDisjunction         : Real -> Tensor Real dims -> Real
  }

VehicleLoss : DifferentiableTensorLogic
VehicleLoss =
  { trueElement                = -1000000
  , falseElement               = 1000000
  , pointwiseNegation          = \x -> -x
  , pointwiseConjunction       = \x y -> max x y
  , pointwiseDisjunction       = \x y -> min x y
  , pointwiseLessThan          = \x y -> x - y
  , pointwiseLessEqualThan     = \x y -> x - y
  , pointwiseGreaterThan       = \x y -> y - x
  , pointwiseGreaterEqualThan  = \x y -> y - x
  , pointwiseEqual             = \x y -> min (x - y) (y - x)
  , pointwiseNotEqual          = \x y -> max (x - y) (y - x)
  , reduceConjunction          = \e xs -> reduceMax e xs
  , reduceDisjunction          = \e xs -> reduceMin e xs
  }

DL2Loss : DifferentiableTensorLogic
DL2Loss =
  { trueElement                = 0
  , falseElement               = 1000000 -- TODO should be infinity
  , pointwiseNegation          = \{dims} x -> (const 1 dims) / x
  , pointwiseConjunction       = \x y -> x + y
  , pointwiseDisjunction       = \x y -> x * y
  , pointwiseLessThan          = \{dims} x y -> max (const 0 dims) (x - y)
  , pointwiseLessEqualThan     = \{dims} x y -> max (const 0 dims) (x - y)
  , pointwiseGreaterThan       = \{dims} x y -> max (const 0 dims) (y - x)
  , pointwiseGreaterEqualThan  = \{dims} x y -> max (const 0 dims) (y - x)
  , pointwiseEqual             = \{dims} x y -> - (max (const 0 dims) (x - y) + max (const 0 dims) (y - x))
  , pointwiseNotEqual          = \{dims} x y -> (max (const 0 dims) (x - y) + max (const 0 dims) (y - x))
  , reduceConjunction          = \e xs -> reduceAdd e xs
  , reduceDisjunction          = \e xs -> reduceMul e xs
  }

STLLoss : DifferentiableTensorLogic
STLLoss =
  { trueElement                = 1000000     -- large positive: fully satisfied
  , falseElement               = -1000000    -- large negative: fully violated
  , pointwiseNegation          = \x -> -x
  , pointwiseConjunction       = \x y -> min x y   -- AND = worst-case robustness
  , pointwiseDisjunction       = \x y -> max x y   -- OR  = best-case robustness
  , pointwiseLessThan          = \x y -> y - x     -- positive when x < y
  , pointwiseLessEqualThan     = \x y -> y - x     -- positive when x <= y
  , pointwiseGreaterThan       = \x y -> x - y     -- positive when x > y
  , pointwiseGreaterEqualThan  = \x y -> x - y     -- positive when x >= y
  , pointwiseEqual             = \x y -> min (x - y) (y - x)   -- = -(|x-y|), 0 when equal
  , pointwiseNotEqual          = \x y -> max (x - y) (y - x)   -- = |x-y|, positive when unequal
  , reduceConjunction          = \e xs -> reduceMin e xs  -- AND-over-vector = min
  , reduceDisjunction          = \e xs -> reduceMax e xs  -- OR-over-vector  = max
  }
