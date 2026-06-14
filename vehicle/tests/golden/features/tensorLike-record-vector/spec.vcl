@tensor
record Track where
  { p : Tensor Real [3]
  , q : Tensor Real [3]
  }

minBound : Track
minBound = { p = [0, 0, 0], q = [0, 0, 0] }

maxBound : Track
maxBound = { p = [1, 1, 1], q = [1, 1, 1] }

@network
f : Track -> Tensor Real [3]

@property
safe : Bool
safe = forall x . minBound <= x <= maxBound => (f x) ! 0 > 0
