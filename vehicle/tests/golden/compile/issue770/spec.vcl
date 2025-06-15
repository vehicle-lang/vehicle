@parameter
n : Nat

@network
f : Tensor Rat [1] -> Tensor Rat [n + 1]

@property
p : Bool
p = f [0] ! 0 >= 0
