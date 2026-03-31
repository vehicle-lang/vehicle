@property
p : Bool
p = forall (x : Tensor Real [2]) . forall i . (1 <= i) => (0.1 <= x ! i)
