@parameter(infer=True)
n : Nat

@dataset
d : Tensor Rat [n]

@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
positive : Bool
positive = forall x in d . f [x] ! 0 > 0
