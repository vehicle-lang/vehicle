@network
f : Tensor Rat [2] -> Tensor Rat [2]

@property
index : Tensor Bool [2]
index = foreach i . f [0, 0] ! i >= 0

@property
safe : Bool
safe = forall x . 0 <= f (foreach i . x ! i + 4.0) ! 0
