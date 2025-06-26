-- Only property `p2` is compiled

add1 : Real -> Real
add1 x = x + 1

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p1 : Bool
p1 = forall x . f x ! 0 >= add1 0


@network
g : Tensor Real [2] -> Tensor Real [1]

@property
p2 : Bool
p2 = forall x . g x ! 0 >= 0
