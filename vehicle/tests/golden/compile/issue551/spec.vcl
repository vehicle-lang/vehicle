@network
f : Tensor Real [2] -> Tensor Real [1]

@property
p : Bool
p = forall x . (if f x ! 0 > 0.5 then 1 else 0) != 1
