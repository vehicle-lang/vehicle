@network
f : Vector Rat 1 -> Vector Rat 1

square : Rat -> Rat
square y = y * y

@property
p : Bool
p = forall (x : Rat) . square (f [x] ! 0) > 0