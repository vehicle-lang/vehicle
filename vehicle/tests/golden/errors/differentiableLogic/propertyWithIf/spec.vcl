@network
f : Tensor Real [1] -> Tensor Real [1]

-- Cannot compile as the `if` condition has gradients.
@property
p : Bool
p = forall (x : Real) . 0.0 <= x <= 1.0 => f [ if x >= 0.5 then x else 0.0 ] ! 0 >= 0.5
