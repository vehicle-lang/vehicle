@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = forall y . (forall x . f [x] != [y]) => y == 0
