@network
f : Tensor Real [1] -> Tensor Real [1]

@property
trivial : Bool
trivial = exists x . f x <= f x

@property
trivialSemantic1 : Bool
trivialSemantic1 = exists x y . f x <= f y and x == y

@network
g : Real -> Real

@property
trivialSemantic2 : Bool
trivialSemantic2 = exists x . g (x + 2) >= g (2 + x)
