@network
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = forall (x : Vector Rat 1) .
  (if f x ! 0 > 0.5 then 1 else 0) == 0
