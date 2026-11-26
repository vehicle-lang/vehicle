-- Tests the error when the types differ underneath a binder.

f : (Nat -> Nat) -> Bool
f g = g 0 > 0

k : (Nat -> Bool) -> Bool
k = f
