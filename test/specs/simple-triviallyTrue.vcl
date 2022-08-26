@network 
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = (forall x . f x ! 0 > 0) or True

@property
multiProperty : Tensor Bool [2, 3]
multiProperty = [[True, True, True], [False, True, False]]