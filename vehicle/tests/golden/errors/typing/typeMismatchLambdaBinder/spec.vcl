-- Tests the error when the type of a lambda binder does not match the expected type.

g : Nat -> Bool
g = \(x : Bool) -> x
