@parameter
n : Nat

@network
f : Vector Rat 1 -> Vector Rat (n + 1)

@property
p : Bool
p = f [0] ! 0 >= 0
