@network
network : Tensor Real [1] -> Tensor Real [1]

@property
prop : Bool
prop = network [1] ! 0 >= 0
