@tensor
record Pair where
  { a : Real
  , b : Real
  }

minBound : Pair
minBound = { a = 0, b = 0 }

maxBound : Pair
maxBound = { a = 1, b = 1 }

@network
f : Pair -> Pair

@property
safeBatch : Vector Bool 3
safeBatch = foreach i . forall x . minBound <= x <= maxBound => (f x).a > 0
