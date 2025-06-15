untyped = [0]

empty : Tensor Rat [0]
empty = []

@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
p : Bool
p = f untyped ! 0 >= 0
