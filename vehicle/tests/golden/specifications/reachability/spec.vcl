@network
f : Tensor Real [2] -> Real

@property
reachable : Bool
reachable = exists x . [0,0] <= x <= [1,1] and f x == 0
