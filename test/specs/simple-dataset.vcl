@network
f : Tensor Rat [1] -> Tensor Rat [4]

@dataset
d : List (Index 4)

@property
spec : Bool
spec = forall i in d . f [0] ! i >= 0