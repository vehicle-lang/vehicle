unused : Bool
unused = forall (x : Rat) . True

@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
expandedExpr : Bool
expandedExpr = forall x . x ! 0 >= f x ! 0

@property
parallel : Bool
parallel = (forall x . f x ! 0 >= 0) and (exists x . f x ! 0 >= 5)
