@network
f : Tensor Rat [1] -> Tensor Rat [1]

existential : Rat -> Bool
existential y = exists x . x >= y

@property
p : Bool
p = forall (y : Rat) . existential y
