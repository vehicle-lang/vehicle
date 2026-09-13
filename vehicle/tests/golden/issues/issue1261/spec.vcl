type Input = Tensor Real [2]

@network
f : Input -> Real

@property
p : Bool
p = forall x . reduceAdd (x * x) <= 0.1 => f x >= 0
