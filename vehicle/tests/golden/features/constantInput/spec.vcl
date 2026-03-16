@network
f : Tensor Real [2] -> Real

@property
spec : Bool
spec = forall x . 0 < x < 1 => f [x , 0.0] >= 0
