f : (Real -> Real) -> Real -> Real
f h x = h x

g : Real -> Real
g x = x + 1

x : Real
x = f g 0
