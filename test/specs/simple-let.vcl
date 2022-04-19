
e1 : Prop
e1 = let x = True, y = False in x and y

e2 : Prop
e2 = let y = 1 in y >= 2

e3 : Nat
e3 = let y = 1 + 1 in y

e4 : Nat -> Nat
e4 = \(x : Nat) -> let y = x in y

-- e5 : Nat -> Nat
-- e5 = let y = \x -> x in y