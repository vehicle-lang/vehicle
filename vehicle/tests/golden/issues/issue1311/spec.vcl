@network
f : Tensor Real [2] -> Real

@property
p : Bool
p = forall (y : Tensor Real [2]) . (exists i . f y >= y ! i)
