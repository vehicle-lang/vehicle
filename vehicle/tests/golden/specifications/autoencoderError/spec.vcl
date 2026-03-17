@network
encode : Tensor Real [5] -> Tensor Real [2]

@network
decode : Tensor Real [2] -> Tensor Real [5]

epsilon : Tensor Real [5]
epsilon = foreach i . 0.1

@property
identity : Bool
identity = forall x . x - epsilon <= decode (encode x) <= x + epsilon
