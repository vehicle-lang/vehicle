network f : Tensor Rat [2] -> Tensor Rat [1]

reachable : Prop
reachable = some x . (let y = f x in y ! 0 == 0)