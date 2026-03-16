@network
classifier : Tensor Real [1, 2] -> Real

@property
oppositeFaces : Bool
oppositeFaces = forall (x : Tensor Real [1, 2]) . [[0,0]] < x < [[1,1]] => classifier (foreach c . x ! c) > 0
