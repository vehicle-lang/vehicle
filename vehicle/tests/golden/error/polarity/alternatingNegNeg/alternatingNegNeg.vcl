@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = forall y . not (not (exists x . f [x] != [y]))
