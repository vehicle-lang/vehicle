@network
f : Tensor Rat [2] -> Tensor Rat [2]

@property
index : Tensor Bool [2]
index = foreach i . f [0, 0] ! i >= 0

-- mkMatrix : Tensor Nat [2,2]
-- mkMatrix = foreach i j . 0
