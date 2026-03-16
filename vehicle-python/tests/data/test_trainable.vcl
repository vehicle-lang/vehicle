@network
network : Tensor Real [1] -> Tensor Real [1]

@property
output_bounded : Bool
output_bounded = forall (x : Real) . 0 <= x <= 1 => network [x] ! 0 <= 5
