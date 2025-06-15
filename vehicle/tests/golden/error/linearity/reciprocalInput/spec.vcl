@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = forall (x : Rat) . f [1 / x] ! 0 > 0
