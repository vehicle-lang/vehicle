@network
f : Tensor Rat [2] -> Tensor Rat [1]

@property
spec : Bool
spec = forall x . f [x , 0.0] ! 0 >= 0