@network
classifier : Tensor Real [1, 2] -> Real

@property
oppositeFaces : Bool
oppositeFaces = forall (x : Tensor Real [1, 2]) . classifier (foreach c . x ! c) > 0
