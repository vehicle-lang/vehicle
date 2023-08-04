@network
f : Vector Rat 2 -> Vector Rat 1

@property
p : Bool
p = forall x . (if f x ! 0 > 0.5 then 1 else 0) != 1
