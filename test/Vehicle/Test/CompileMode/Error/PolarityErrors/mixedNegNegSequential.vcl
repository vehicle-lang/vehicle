@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = forall y . not (not (exists x . f [x] != [y]))