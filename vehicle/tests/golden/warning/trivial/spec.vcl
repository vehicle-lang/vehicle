@network
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = (forall x . f x ! 0 > 0) or True

@property
multiProperty : Tensor Bool [3, 1]
multiProperty = [[True], [forall x . f x ! 0 > 0], [False]]
