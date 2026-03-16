@network
f : Real -> Real

@property
p : Tensor Bool [2]
p = [forall x . 0 < x < 1 => f x <= 0, True]
