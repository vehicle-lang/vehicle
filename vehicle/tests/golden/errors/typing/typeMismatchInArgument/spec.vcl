-- Tests the error when part of a function argument's type does not match the expected type.

g : Vector Real 2 -> Real
g x = x ! 0

h : Vector Real 3
h = [1,2,3]

k : Real
k = g h
