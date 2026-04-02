@tensor
record Pair where
  { a : Real
  , b : Real
  }

-- unused : Bool
-- unused = forall (x : Real) . True

@network
f : Pair -> Pair

-- @property
-- expandedExpr : Bool
-- expandedExpr = forall x . 0 < x < 1 => x >= f x

-- @property
-- parallel : Bool
-- parallel = (forall x . 0 < x < 1 => f x >= 0) and (exists y . 0 < y < 1 and f y >= 5)

@property
parallel : Bool
parallel = (forall x . f x > x)