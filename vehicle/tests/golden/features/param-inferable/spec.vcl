@parameter(infer=True)
n : Nat

@dataset
d : Tensor Real [n]

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
positive : Bool
positive = forall x in d . f [x] ! 0 > 0
