@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
monotonic : Bool
monotonic = forall x1 x2 . (x1 <= x2) => (f [x1] ! 0 <= f [x2] ! 0)
