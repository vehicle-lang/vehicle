@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = forall y . not (forall x . f [x] != [y])
