@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = forall (x : Real) . (if x * x > 0 then True else False)
