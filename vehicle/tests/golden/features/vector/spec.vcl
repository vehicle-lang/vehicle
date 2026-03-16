untyped = [0]

empty : Tensor Real [0]
empty = []

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = f untyped ! 0 >= 0
