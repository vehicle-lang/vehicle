@tensor
record Pair where
  { a : Real
  , b : Real
  }

minBound : Pair
minBound = { a = 0, b = 0 }

@network
f : Pair -> Pair

@property
p : Bool
p = forall x . (minBound + minBound) <= x => (f x).a > x.a
