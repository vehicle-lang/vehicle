@network
f : Tensor Real [1] -> Tensor Real [2]

@property
p : Bool
p = forall x . 0 <= min (f x ! 0) (f x ! 1) and max (f x ! 0) (f x ! 1) <= 1
