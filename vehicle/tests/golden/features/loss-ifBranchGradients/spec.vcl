@network
f : Tensor Real [1] -> Tensor Real [1]

@parameter
n : Nat

@property
p : Bool
p = forall (x : Real) . 0.0 <= x <= 1.0 => f [ if n == 1 then x else 0.0 ] ! 0 >= 0.5
