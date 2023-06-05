@network
f : Vector Rat 2 -> Vector Rat 1

@property
robustAround : Bool
robustAround = forall p . (if f p ! 0 > 0.5 then 1 else 0) == 1
