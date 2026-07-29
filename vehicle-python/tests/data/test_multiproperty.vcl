@network
f : Real -> Real

@property
p : Vector Bool 2
p = [exists x . 0 < x < 1 and (f x >= 0 or f x <= 1), forall x . 0 < x < 1 => f x <= 1]
