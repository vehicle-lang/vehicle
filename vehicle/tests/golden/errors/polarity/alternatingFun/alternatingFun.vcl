@network
f : Tensor Real [1] -> Tensor Real [1]

existential : Real -> Bool
existential y = exists x . x >= y

@property
p : Bool
p = forall (y : Real) . existential y
