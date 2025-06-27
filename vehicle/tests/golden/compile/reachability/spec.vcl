@network
f : Tensor Real [2] -> Tensor Real [1]

@property
reachable : Bool
reachable = exists x . f x == [0]
