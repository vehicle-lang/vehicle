@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = forall y . exists x . f [x] == [y]
