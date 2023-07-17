@network
network : Tensor Rat [1] -> Tensor Rat [1]

@property
prop : Bool
prop = network [1] ! 0 >= 0
