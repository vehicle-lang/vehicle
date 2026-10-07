@network
f : Tensor Real [2] -> Tensor Real [1]

-- The index quantified over is symbolic, so the comparison cannot be evaluated.
@property
p : Bool
p =
  forall (y : Tensor Real [2]) .
    (forall i . 0.0 <= y ! i <= 1.0) => (exists i . f y ! 0 >= y ! i)
