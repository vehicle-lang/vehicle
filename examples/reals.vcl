one : Int
one = 1

oneReal : Real
oneReal = 1.0

sum : Real
sum = 1.0 + 2.0

-- TODO: the type of (==) in DSL.hs is incorrect
dataset eq : forall {t : Type 0}. forall {o : Type 0}. forall {e : (HasEq t o)}. t -> t -> o

-- this works, up to filling in the typeclass constraints
isOne : Real -> Prop
isOne x = eq 1.0 x

isOneAgain : Real -> Prop
isOneAgain x = 1.0 == x

{-
-- this fails at the moment because we aren't solving flex-flex
-- problems properly. It is also underconstrained -- there is nothing
-- to say whether the numbers are to be interpreted as Reals or Ints.
prop : Prop
prop = eq 1.0 2.0
-}
