network encode : Tensor Rat [5] -> Tensor Rat [2]
network decode : Tensor Rat [2] -> Tensor Rat [5]

identity : Prop
identity = every x . decode (encode x) == x