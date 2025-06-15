@parameter
n : Nat

@network
f : Tensor Rat [n] -> Tensor Rat [1]

@property
p : Bool
p = f (foreach i . 0) ! 0 >= 0
