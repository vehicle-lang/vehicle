@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = forall (x : Tensor Real [1]) .
  (if f x ! 0 > 0.5 then 1 else 0) == 0
