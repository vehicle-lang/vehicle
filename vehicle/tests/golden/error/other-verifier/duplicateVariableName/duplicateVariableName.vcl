@network
f : Vector Rat 1 -> Vector Rat 1

@property
property : Bool
property = forall (x : Rat) . x >= 0 and (forall x . f x >= 0)
