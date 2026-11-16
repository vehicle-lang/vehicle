@network
f : Tensor Real [2] -> Tensor Real [1]

@property
direct : Bool
direct = forall (x : Real) . 0.0 <= x <= 1.0 => (exists (z : Real) . f [x, z] ! 0 >= 0.5)

@property
underConjunction : Bool
underConjunction = forall (x : Real) . 0.0 <= x <= 1.0 => (f [x, x] ! 0 >= 0.5 and (exists (z : Real) . f [x, z] ! 0 >= 0.5))

@property
underDisjunction : Bool
underDisjunction = forall (x : Real) . 0.0 <= x <= 1.0 => (f [x, x] ! 0 >= 0.5 or (exists (z : Real) . f [x, z] ! 0 >= 0.5))
