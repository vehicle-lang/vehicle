@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
increasing : Bool
increasing = forall x . x <= f [x] ! 0