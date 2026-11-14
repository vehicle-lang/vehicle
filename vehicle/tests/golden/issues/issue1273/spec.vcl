-- A comparison relating slices of two separately quantified tensors.
@network
classifier : Tensor Real [2] -> Tensor Real [1]

@property
p : Bool
p = forall x y.
  0.0 <= x ! 0 <= 1.0 and
  0.0 <= y ! 0 <= 1.0 and
  x ! 0 >= y ! 0 => classifier x ! 0 >= classifier y ! 0
