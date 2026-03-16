@network
f : Real -> Real

@property
p : Bool
p = if (forall x . 0 < x < 1 => f x > 0) then True else False
