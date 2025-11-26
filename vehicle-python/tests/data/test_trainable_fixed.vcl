@network
network : Tensor Real [1] -> Tensor Real [1]

@property
bounded_corrected : Bool
bounded_corrected = forall (x : Real) . 0 < x < 1  => 0 < (network [x] @ 0) < 1
