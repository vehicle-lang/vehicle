unused : Bool
unused = forall (x : Rat) . True

@network
f : Vector Rat 1 -> Vector Rat 1

@property
expandedExpr : Bool
expandedExpr = forall x . x ! 0 >= f x ! 0

@property
sequential : Bool
sequential = forall x y . f x ! 0 >= f y ! 0

@property
parallel : Bool
parallel = (forall x . f x ! 0 >= 0) and (exists x . f x ! 0 >= 5)
