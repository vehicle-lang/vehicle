

y : forall {t : Type 0}. (t -> t) -> t
y {t} f = ((\x -> f (x (x : t))) : (t -> t) -> t) (\x -> f ((x : t -> t) x))

-- if the unifier expanded out definitions, then this would not terminate when trying to get to WHNF.
test : forall {t : Type 0}. forall {pred : (t -> Type 0)}. forall {x : t}. pred x -> pred (y {t} (\x -> x))
test p = p
