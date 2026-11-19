-- Tests that bounds on the input of one network are shared with the inputs of
-- other networks whose inputs are known to be equal to it. Most solvers require
-- every network input to be bounded, but by default the bounds are only
-- attached to one of the equal inputs.

@network
f : Tensor Real [2, 3] -> Tensor Real [1]

@network
g : Tensor Real [2, 3] -> Tensor Real [1]

@network
h : Tensor Real [2, 3] -> Tensor Real [1]

-- The bounds and the equality are both over the whole input tensors.
@property
sameDimension : Bool
sameDimension = forall x .
  (forall i j . 0 <= x ! i ! j <= 1) =>
  f x ! 0 <= g x ! 0

-- The bounds are over the rows of the input tensor and the equality is over
-- the whole input tensors.
@property
boundsLowerDimension : Bool
boundsLowerDimension = forall x .
  (forall i . [0, 0, 0] <= x ! i <= [1, 2, 3]) =>
  f x ! 0 <= g x ! 0

-- The bounds are over the whole input tensor and the equalities are over the
-- rows of the input tensors.
@property
boundsHigherDimension : Bool
boundsHigherDimension = forall x .
  ([[0, 0, 0], [0, 0, 0]] <= x <= [[1, 2, 3], [4, 5, 6]]) =>
  f x ! 0 <= g [x ! 1, x ! 0] ! 0

-- Each element of the second network's input is equal to an element of the
-- first network's input in a different position.
@property
elementEqualities : Bool
elementEqualities = forall x .
  (forall i j . -1 <= x ! i ! j <= 1) =>
  f x ! 0 <= g [[x ! 1 ! 2, x ! 1 ! 1, x ! 1 ! 0], [x ! 0 ! 2, x ! 0 ! 1, x ! 0 ! 0]] ! 0

-- Bounds are shared with the inputs of more than one other network.
@property
multipleNetworks : Bool
multipleNetworks = forall x .
  (forall i j . 0 <= x ! i ! j <= 1) =>
  f x ! 0 <= g x ! 0 and g x ! 0 <= h x ! 0

-- Only some of the second network's input is equal to the first network's
-- input, and the rest is bounded separately.
@property
partialEquality : Bool
partialEquality = forall x (y : Tensor Real [3]) .
  (forall i j . 0 <= x ! i ! j <= 1) and
  (forall j . 2 <= y ! j <= 3) =>
  f x ! 0 <= g [x ! 0, y] ! 0

-- Separate lower and upper bounds at different dimensions are each shared
-- and then combined.
@property
combinedBounds : Bool
combinedBounds = forall x .
  (forall i j . 0 <= x ! i ! j) and
  (forall i . x ! i <= [1, 2, 3]) =>
  f x ! 0 <= g x ! 0
