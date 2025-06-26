@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = forall y . (forall x . f [x] != [y]) => y == 0
