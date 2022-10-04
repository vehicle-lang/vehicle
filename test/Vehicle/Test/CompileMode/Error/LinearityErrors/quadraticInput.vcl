@network
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = forall (x : Rat) . f [x * x] ! 0 > 0