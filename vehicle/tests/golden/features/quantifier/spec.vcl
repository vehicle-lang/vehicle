@tensor
record Pair where
  { a : Real
  , b : Real
  }

-- unused : Bool
-- unused = forall (x : Real) . True

@network
f : Pair -> Pair

@property
p : Bool
p = (forall x . (f x) > x)