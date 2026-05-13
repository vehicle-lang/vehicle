@network
f : Tensor Real [4] -> Tensor Real [4]

@property
p : Bool
p = f [0.0, 0.0, 0.0, 0.0] <= const 10.0 [4]
