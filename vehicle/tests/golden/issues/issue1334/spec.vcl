@network
f : Tensor Real [1] -> Tensor Real [1]

flags : Vector Nat 2
flags = [1, 0]

@property
p : Bool
p = forall x . 0 <= x ! 0 <= 1 and flags ! 0 == 1 => f x ! 0 >= 0
