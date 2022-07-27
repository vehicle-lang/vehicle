network f : Tensor Rat [2] -> Tensor Rat [2]

index : Tensor Bool [2]
index = foreach i . f [0, 0] ! i >= 0