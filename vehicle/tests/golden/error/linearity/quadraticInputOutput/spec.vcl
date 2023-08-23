@network
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = forall (x : Rat) . (f [x] ! 0) * x > 0
