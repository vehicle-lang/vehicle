network A : Type 0
network B : A -> Type 0

network fn : forall {x : A}. B x -> B x
