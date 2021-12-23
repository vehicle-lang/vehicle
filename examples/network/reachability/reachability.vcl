network f : Tensor Rat [2] -> Tensor Rat [1]

reachable : Prop
reachable = some x . f x ! 0 == 0