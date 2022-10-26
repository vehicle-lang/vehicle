-- Only property `p2` is compiled

add1 : Rat -> Rat
add1 x = x + 1

@network
f : Vector Rat 1 -> Vector Rat 1

@property
p1 : Bool
p1 = forall x . f x ! 0 >= add1 0


@network
g : Vector Rat 2 -> Vector Rat 1

@property
p2 : Bool
p2 = forall x . g x ! 0 >= 0
