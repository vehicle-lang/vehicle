@network
network : Tensor Real [1] -> Tensor Real [1]

@property
bounded : Bool
bounded = forall x . 0 < x < 1  => 0 < network [x] ! 0 < 1
