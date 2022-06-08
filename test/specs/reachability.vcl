network f : Tensor Rat [2] -> Tensor Rat [1]

reachable : Bool
reachable = exists x . f x == [0]