-- Tests that user-defined types are not unfolded when displaying a type mismatch.

type Pair = Vector Real 2

p : Pair
p = [0, 0]

v : Vector Nat 2
v = p
