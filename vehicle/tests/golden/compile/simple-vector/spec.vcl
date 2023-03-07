@network
f : Vector Rat 1 -> Vector Rat 1

empty : Vector Rat 0
empty = []

@property
p : Bool
p = f [0] ! 0 >= 0
