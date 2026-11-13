@network
f : Tensor Real [1] -> Tensor Real [1]

@parameter
strict : Bool

@property
conjunction : Bool
conjunction = forall (x : Real) . 0.0 <= x <= 1.0 => (strict and f [x] ! 0 >= 0.5)

@property
condition : Bool
condition = forall (x : Real) . 0.0 <= x <= 1.0 => (if strict then f [x] ! 0 >= 0.5 else f [x] ! 0 <= 0.5)
