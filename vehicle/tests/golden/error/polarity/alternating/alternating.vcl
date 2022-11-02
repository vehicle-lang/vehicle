@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = forall y . exists x . f [x] == [y]
