@network
f : Tensor Real [1] -> Tensor Real [1]

@property
monotonic : Bool
monotonic = forall x1 x2 . (x1 <= x2) => (f [x1] <= f [x2])
