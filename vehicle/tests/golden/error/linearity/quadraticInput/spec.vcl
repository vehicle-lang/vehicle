@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = forall (x : Rat) . f [x * x] ! 0 > 0
