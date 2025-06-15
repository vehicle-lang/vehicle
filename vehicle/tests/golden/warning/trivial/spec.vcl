@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = (forall x . f x ! 0 > 0) or True

@property
multiProperty : Tensor Bool [3, 1]
multiProperty = [[True], [forall x . f x ! 0 > 0], [False]]
