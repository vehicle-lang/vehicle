untyped = [0]

empty : Vector Rat 0
empty = []

@network
f : Vector Rat 1 -> Vector Rat 1

@property
p : Bool
p = f untyped ! 0 >= 0
