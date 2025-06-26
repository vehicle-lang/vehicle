@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p : Bool
p = if (forall x . f x ! 0 > 0) then True else False
