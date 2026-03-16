@parameter
n : Nat

@network
f : Tensor Real [n] -> Tensor Real [1]

@property
p : Bool
p = f (foreach i . 0) ! 0 >= 0
