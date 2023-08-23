@network
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = forall (x : Rat) . f [1 / x] ! 0 > 0
