@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
trivial : Bool
trivial = f [0] ! 0 > 0
