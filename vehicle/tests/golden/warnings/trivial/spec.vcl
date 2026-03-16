@network
f : Real -> Real

@property
p : Bool
p = (forall x . f x > 0) or True

@property
multiProperty : Tensor Bool [3, 1]
multiProperty = [[True], [forall x . 0 < x < 1 => f x > 0], [False]]
