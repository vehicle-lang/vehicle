@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = forall x . forall y in [f x ! 0] . y > 0
