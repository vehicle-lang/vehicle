@network
f : Tensor Rat [1] -> Tensor Rat [2]

@property
p : Bool
p = forall x . 0 <= min (f x ! 0) (f x ! 1) and max (f x ! 0) (f x ! 1) <= 1
