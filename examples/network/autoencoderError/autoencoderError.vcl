network encode : Tensor Rat [5] -> Tensor Rat [2]
network decode : Tensor Rat [2] -> Tensor Rat [5]

identity : Prop
identity = forall x . decode (encode x) == x