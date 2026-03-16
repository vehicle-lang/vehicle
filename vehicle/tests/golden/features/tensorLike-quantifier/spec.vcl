@tensor
record Pair where
  { a : Real
  , b : Real
  }

@network
f : Pair -> Pair

@property
simple : Bool
simple = forall x . 0 <= x.a <= 1 => x.b <= (f x).b

@property
parallel : Bool
parallel = (forall x . 0 < x.a < 1 => (f x).a >= 0) and (exists y . 0 < y.b < 1 and (f y).b >= 5)
