@network
f : Vector Rat 1 -> Vector Rat 1

square : Rat -> Rat
square y = y * y

@property
p : Bool
p = forall (x : Rat) . f [square x] ! 0 > 0