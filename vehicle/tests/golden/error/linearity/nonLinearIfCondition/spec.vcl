@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = forall (x : Rat) . (if x * x > 0 then True else False)
