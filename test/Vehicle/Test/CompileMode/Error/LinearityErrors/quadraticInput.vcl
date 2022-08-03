network f : Tensor Rat [1] -> Tensor Rat [1]

property : Bool
property = forall (x : Rat) . f [x * x] ! 0 > 0