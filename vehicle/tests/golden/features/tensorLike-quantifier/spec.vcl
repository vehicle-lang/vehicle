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
p = forall x . minBound <= x <= maxBound => (f x).a > x.a

@property
parallel : Bool
parallel = (forall x . minBound <= x <= maxBound => (f x).a > 0) and (exists y . minBound <= y <= maxBound and (f y).b >= 5)
