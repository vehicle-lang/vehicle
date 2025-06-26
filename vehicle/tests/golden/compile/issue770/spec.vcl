@parameter
n : Nat

@network
f : Tensor Real [1] -> Tensor Real [n + 1]

@property
p : Bool
p = f [0] ! 0 >= 0
