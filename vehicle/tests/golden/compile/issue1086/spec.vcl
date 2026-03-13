@network
f : Tensor Real [1] -> Real

@property
p : Bool
p = forall x . (0 <= x ! 0 <= 1) => f x >= 0
