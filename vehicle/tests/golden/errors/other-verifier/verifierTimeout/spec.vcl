@network
f : Real -> Real

@property
p1 : Bool
p1 = exists x . 0 <= x <= 1 and f x >= 0

@property
p2 : Bool
p2 = p1
