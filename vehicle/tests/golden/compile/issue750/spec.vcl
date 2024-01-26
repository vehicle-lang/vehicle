@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = exists x y . f x ! 0 <= f y ! 0 and x == y and 0 <= x ! 0 <= 1 and 0 <= y ! 0 <= 1
