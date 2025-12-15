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
-- TensorLike
--------------------------------------------------------------------------------

record TensorLike r t dims {{isTensorType : IsTensorType t dims}} where
  { toTensor         : r -> Tensor t dims
  , fromTensor       : Tensor t dims -> r
  }

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

existsIndex : forallT {n} . (Index n -> Bool) -> Bool
existsIndex f = reduceOr False (foreach i . f i)

forallIndex : forallT {n} . (Index n -> Bool) -> Bool
forallIndex f = reduceAnd True (foreach i . f i)

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
