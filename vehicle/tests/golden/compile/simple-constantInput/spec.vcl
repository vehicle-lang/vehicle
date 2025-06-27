@network
f : Tensor Real [2] -> Tensor Real [1]

@property
spec : Bool
spec = forall x . f [x , 0.0] ! 0 >= 0
