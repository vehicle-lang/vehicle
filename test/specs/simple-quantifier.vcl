network f : Vector Rat 1 -> Vector Rat 1

-- bool : Bool
-- bool = forall (x : Int) . True

expandedExpr : Bool
expandedExpr = forall x . x ! 0 >= f x ! 0

sequential : Bool
sequential = forall x y . f x ! 0 >= f y ! 0

-- See #102
-- parallel : Bool
-- parallel = (forall x . f x ! 0 >= 0) and (exists x . f x ! 0 >= 5)