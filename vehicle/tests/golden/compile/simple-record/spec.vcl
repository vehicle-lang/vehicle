record Pair t1 t2 where
  { a : t1
  , b : t2
  }

{-
Elaborated to

(morally)
Pair : \forall t1 t2 -> Set
Pair = \t1 t2 -> { a : t1, b : t2 }

a : \forall {t1} {t2} -> Pair t1 t2 -> t1
a {t1} {t2} r = Proj (Pair t1 t2) r a

b : \forall {t1} {t2} -> Pair t1 t2 -> t2
b {t1} {t2} r = Proj (Pair t1 t2) r b
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
-- Becomes f [a pair, b pair]
