@network
f : Tensor Real [1] -> Tensor Real [1]

square : Real -> Real
square y = y * y

@property
p : Bool
p = forall (x : Real) . square (f [x] ! 0) > 0
