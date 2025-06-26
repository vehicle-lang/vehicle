@network
f : Tensor Real [1] -> Tensor Real [1]

@property
increasing : Bool
increasing = forall x . x <= f [x] ! 0
