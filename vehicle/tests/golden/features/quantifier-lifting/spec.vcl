@network
f: Real -> Real

@property
existBelowAnd: Bool
existBelowAnd = (exists x1 . 0 < x1 < 1 and f x1 >= 1) and (exists x2 . 1 < x2 < 2 and f x2 >= 2)

@property
forallBelowAnd: Bool
forallBelowAnd = (forall x1 . 0 < x1 < 1 => f x1 >= 1) and (forall x2 . 1 < x2 < 2 => f x2 >= 2)

-- Unblocking error
-- @property
-- existsInsideExists: Bool
-- existsInsideExists = exists (x1 : Real) . (0 < x1 < 1 and (exists (x2 : Real) . 0 < x2 < x1))

-- Unblocking error
-- @property
-- forallInsideForall: Bool
-- forallInsideForall = forall (x1 : Real) . (0 < x1 < 1 and (forall (x2 : Real) . 0 < x2 < x1))

@property
existsAndForallDifferentScopes: Bool
existsAndForallDifferentScopes = (exists (x1 : Real) . 0 < x1 < 1 and f x1 >= 1) and (forall (x2 : Real) . 1 < x2 < 2 => f x2 >= 2)
