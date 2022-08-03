network encode : Tensor Rat [5] -> Tensor Rat [2]
network decode : Tensor Rat [2] -> Tensor Rat [5]

epsilon : Tensor Rat [5]
epsilon = foreach i . 0.1

identity : Bool
identity = forall x i . (x - epsilon) ! i <= decode (encode x) ! i <= (x + epsilon) ! i