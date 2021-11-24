network f : Tensor Real [2] -> Tensor Real [1]

reachable : Prop
reachable = some x . (let y = f x in y ! 0 == 0)