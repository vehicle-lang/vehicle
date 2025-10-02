-- Only property `p2` is compiled

add1 : Real -> Real
add1 x = x + 1

@network
f : Real -> Real

@property
p1 : Bool
p1 = forall x . 0 < x < 1 => f x >= add1 0

@network
g : Real -> Real

@property
p2 : Bool
p2 = forall x . 0 < x < 1 => g x >= 0
