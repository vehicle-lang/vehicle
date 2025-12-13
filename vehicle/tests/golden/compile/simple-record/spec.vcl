record Pair x y where
  { a : x
  , b : y
  }

{-
Delaborated to

Pair : \forall x y -> Set
Pair = \x y -> { a : x, b : y }

a : \forall {x} {y} -> Pair x y -> x
a {x} {y} r = Proj (Pair x y) r a

a : \forall {x} {y} -> Pair x y -> y
a {x} {y} r = Proj (Pair x y) r b
-}

RealPair : Type
RealPair = Pair Real Real

pair : RealPair
pair = { a = 1, b = 1 }

@network
f : Tensor Real [2] -> Tensor Real [2]

@property
safe : Bool
safe = f [pair.a, pair.b] ! 0 > 0
