network f : Tensor Rat [1] -> Tensor Rat [1]

square : Rat -> Rat
square y = y * y

property : Bool
property = forall (x : Rat) . f [square x] ! 0 > 0