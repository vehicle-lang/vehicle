@network
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = if (forall x . f x ! 0 > 0) then True else False
