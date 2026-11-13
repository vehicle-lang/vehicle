@network
f : Tensor Real [2] -> Tensor Real [1]

@property
p : Bool
p = forall (x : Real) . 0.0 <= x <= 1.0 => (let y = x in f [y, y] ! 0 >= 0.5)
