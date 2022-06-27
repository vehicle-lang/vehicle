network f : Tensor Rat [2] -> Tensor Rat [2]

index : Tensor Bool [2]
index = foreach i . f [0, 0] ! i >= 0

inTensor : Tensor Bool [2]
inTensor = foreach x in [0, 1] . f [ x , x ] ! 0 >= 0