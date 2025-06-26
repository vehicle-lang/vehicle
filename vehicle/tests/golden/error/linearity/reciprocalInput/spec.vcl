@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = forall (x : Real) . f [1 / x] ! 0 > 0
