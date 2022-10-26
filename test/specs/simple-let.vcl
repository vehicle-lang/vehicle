@property
e1 : Bool
e1 = let x = True, y = False in x and y

@property
e2 : Bool
e2 = let y = 1 in y >= 2

e3 : Nat
e3 = let y = 1 + 1 in y

lamLet : Nat -> Nat
lamLet = \(x : Nat) -> let y = x in y

letLam : Nat -> Nat
letLam = let y = \x -> x in y

@property
forallLet : Bool
forallLet = forall (x : Nat) . let y = x in y == 1

@property
letForall : Bool
letForall = let y = 1 in forall (x : Nat) . y == x

@property
forallInLet : Bool
forallInLet = forall x in [1] . let y = x in y == 1

@property
letForallIn : Bool
letForallIn = let y = 1 in forall x in [1] . y == x
