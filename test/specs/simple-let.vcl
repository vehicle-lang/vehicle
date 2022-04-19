
e1 : Prop
e1 = let x = True, y = False in x and y

e2 : Prop
e2 = let y = 1 in y >= 2

e3 : Nat
e3 = let y = 1 + 1 in y

lamLet : Nat -> Nat
lamLet = \(x : Nat) -> let y = x in y

-- See #89
-- letLam : Nat -> Nat
-- letLam = let y = \x -> x in y

forallLet : Prop
forallLet = forall x . let y = x in y == 1

letForall : Prop
letForall = let y = 1 in forall x . y == x

forallInLet : Prop
forallInLet = forall x in ([1] : List Nat) . let y = x in y == 1

letForallIn : Prop
letForallIn = let y = 1 in forall x in ([1] : List Nat) . y == x