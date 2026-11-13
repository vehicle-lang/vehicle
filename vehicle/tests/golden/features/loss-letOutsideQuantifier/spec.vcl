@network
f : Tensor Real [2] -> Tensor Real [1]

@property
p : Bool
p = let bound = 0.5 in forall (x : Real) . 0.0 <= x <= 1.0 => f [x, x] ! 0 >= bound
