@parameter
n : Nat

@network
f : Vector Rat n -> Vector Rat 1

@property
p : Bool
p = f (foreach i . 0) ! 0 >= 0
