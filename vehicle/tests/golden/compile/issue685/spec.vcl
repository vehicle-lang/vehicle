@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = forall x . forall y in [f x ! 0] . y > 0
