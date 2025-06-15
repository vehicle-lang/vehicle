@network
f : Tensor Rat [2] -> Tensor Rat [1]

dataset : List (Tensor Rat [2])
dataset = [[0.5,1.0]]

@property
empty : Bool
empty = forall x in dataset . True

@property
double : Bool
double = forall x in dataset . forall y in dataset . x == y

@property
forallForallIn : Bool
forallForallIn = forall x . forall y in dataset . x == y

@property
forallInForall : Bool
forallInForall = forall x in dataset . forall y . f x != y
