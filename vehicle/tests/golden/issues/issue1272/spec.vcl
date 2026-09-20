@network
classifier : Tensor Real [2] -> Real

@property
p : Bool
p = forall x . x ! 0 >= x ! 1 => classifier x >= 0.5
