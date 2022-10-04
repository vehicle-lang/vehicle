@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
bounded : Bool
bounded = forall x . 0 < x < 1  => 0 < f [x] ! 0 < 1
