
e1 : Bool
e1 = let x = True, y = False in x and y

e2 : Bool
e2 = let y = 1 in y >= 2

e3 : Nat
e3 = let y = 1 + 1 in y

lamLet : Nat -> Nat
lamLet = \(x : Nat) -> let y = x in y

-- See #89
-- letLam : Nat -> Nat
-- letLam = let y = \x -> x in y

forallLet : Bool
forallLet = forall x . let y = x in y == 1

letForall : Bool
letForall = let y = 1 in forall x . y == x

forallInLet : Bool
forallInLet = forall x in [1] . let y = x in y == 1

letForallIn : Bool
letForallIn = let y = 1 in forall x in [1] . y == x