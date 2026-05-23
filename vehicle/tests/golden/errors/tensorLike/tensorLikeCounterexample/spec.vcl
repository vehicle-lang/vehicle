@tensor
record Pair where
  { a : Real
  , b : Real
  }

minBound : Pair
minBound = { a = 0, b = 0 }

maxBound : Pair
maxBound = { a = 10, b = 10 }

@network
f : Pair -> Pair

@property
p : Bool
p = forall x . (minBound + minBound) <= x <= maxBound => (f x).a > x.a