@network
network : Tensor Real [1] -> Tensor Real [1]

@property
positive_output : Bool
positive_output = network [0.5] ! 0 > 0.5
