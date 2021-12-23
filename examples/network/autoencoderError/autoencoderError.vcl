network encode : Tensor Rat [1] -> Tensor Rat [1]
network decode : Tensor Rat [1] -> Tensor Rat [1]

identity : Prop
identity = every x . decode (encode x) == x