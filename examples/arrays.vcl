network nat : Type 0
network plus : nat -> nat -> nat
network eq : nat -> nat -> Constraint
network lt : nat -> nat -> Constraint

network array : nat -> Type 0 -> Type 0

network lookup : forall {t   : Type 0}.
                 forall {len : nat}.
                 forall (arr : (array len t)).
		 forall (idx : nat).
		 forall {ok  : (lt idx len)}.
		 t

testLookup : array 10 Real -> Real
testLookup arr = lookup arr 5
