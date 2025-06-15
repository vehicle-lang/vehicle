@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
property : Bool
property = forall (x : Rat) . x >= 0 and (forall x . f x >= 0)
