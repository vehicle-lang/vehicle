network eq : forall {t : Type 0}.
             forall {p : Type 0}.
             forall {c : (HasEq t p)}.
	     t -> t -> p

-- TODO: why doesn't this work properly?
eqInProp : forall {t : Type 0}. forall {c : (HasEq t Bool)}. t -> t -> Bool
eqInProp {t} {c} = eq {t}
