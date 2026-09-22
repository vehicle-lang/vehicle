-- `x ! 0 <= x ! i` becomes `const (x ! 0) [3] <= x`, which cannot bound `x`.

@network
f : Tensor Real [3] -> Tensor Real [1]

@property
p : Bool
p =
  forall (x : Tensor Real [3]) .
    (forall i . 0.0 <= x ! i <= 1.0) and (forall i . x ! 0 <= x ! i) =>
      f x ! 0 >= 0.5

-- A bound for `x` would mention `y`, which is bound inside it.
@property
q : Bool
q =
  forall (x : Tensor Real [3]) . forall (y : Tensor Real [3]) .
    (forall i . 0.0 <= x ! i <= 1.0) and
    (forall i . 0.0 <= y ! i <= 1.0) and
    (forall i . x ! i <= y ! 0) =>
      f x ! 0 >= 0.5
