f : (Rat -> Rat) -> Rat -> Rat
f h x = h x

g : Rat -> Rat
g x = x + 1

x : Rat
x = f g 0
