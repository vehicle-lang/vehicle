@network
f : Tensor Real [1] -> Tensor Real [1]

@property
increasing : Bool
increasing = forall (x : Real) . (0 < x < 1) => (x <= (f [x]) ! 0)
