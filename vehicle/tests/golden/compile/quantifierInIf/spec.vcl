@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = if (forall x . f x ! 0 > 0) then True else False
