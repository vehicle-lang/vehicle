-- A comparison relating slices of two separately quantified tensors.

type In = Tensor Real [4]
type Out = Tensor Real [2]

@network
classifier : In -> Out

@property
p : Bool
p =
  forall (x : In) . forall (y : In) .
    (forall i . 0.0 <= x ! i <= 1.0) and
    (forall i . 0.0 <= y ! i <= 1.0) and
    x ! 0 >= y ! 1 =>
      classifier x ! 0 >= classifier y ! 1
