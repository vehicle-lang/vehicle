unused : Bool
unused = forall (x : Real) . True

@network
f : Real -> Real

@property
expandedExpr : Bool
expandedExpr = forall x . 0 < x < 1 => x >= f x

@property
parallel : Bool
parallel = (forall x . 0 < x < 1 => f x >= 0) and (exists y . 0 < y < 1 and f y >= 5)
