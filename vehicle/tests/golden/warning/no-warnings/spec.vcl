@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = forall x . f x ! 0 <= 0
