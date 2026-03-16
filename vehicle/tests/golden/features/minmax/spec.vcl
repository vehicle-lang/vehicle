@network
f : Real -> Tensor Real [2]

@property
p : Bool
p = forall x . 0 < x < 1 => (0 <= min (f x ! 0) (f x ! 1) and max (f x ! 0) (f x ! 1) <= 1)
