From mathcomp Require Import ssralg ssrfun seq tuple fintype reals order.
Open Scope ring_scope.
Open Scope form_scope.

Declare Scope tensor_scope.
Delimit Scope tensor_scope with t.

Definition tensor (A : Type) : seq nat -> Type :=
    foldr tuple_of A.

Definition tensor_of {A} : A -> tensor A [::] := id.

Definition stack {A d} : d.-tuple (tensor A nil) -> tensor A (d :: nil) := id.

Definition unstack {A d} : tensor A (d :: nil) -> d.-tuple (tensor A nil) := id.

Definition foreach {A d} (f : 'I_d -> tensor A [::]) : tensor A ([:: d]) :=
    [tuple f i | i < d].

Fixpoint const {A} (v : A) (ds : list nat) : tensor A ds :=
    match ds with
    | [::] => v
    | d :: ds => foreach (fun=> const v ds)
    end.

Local Fixpoint map {A B ds} (f : A -> B) : tensor A ds -> tensor B ds :=
    match ds with
    | [::] => f
    | d :: ds => map_tuple (map f)
    end.

Fixpoint zip {A B ds} : tensor A ds -> tensor B ds -> tensor (A * B) ds :=
    match ds with
    | [::] => pair
    | d :: ds => fun xs ys => [tuple zip (tnth xs i) (tnth ys i) | i < d]
    end.

Definition zipWith {A B C ds} (f : A -> B -> C) (xs : tensor A ds) (ys : tensor B ds) : tensor C ds :=
    map (uncurry f) (zip xs ys).

Fixpoint toList {A ds} : tensor A ds -> list A :=
    match ds with
    | [::] => fun (t : A) => [:: t]
    | d :: ds => fun t => flatten (seq.map toList t)
    end.

Definition reduce {A B ds} (f : A -> B -> B) (a : B) (t : tensor A ds) : B := foldr f a (toList t).
Definition reduceAnd {ds} : tensor bool ds -> bool := reduce andb true.
Definition reduceOr {ds} : tensor bool ds -> bool := reduce orb false.

Definition pointwise {A B ds} (f : A -> B -> Prop) (xs : tensor A ds) (ys : tensor B ds) : Prop :=
    reduce and True (zipWith f xs ys).

Section TensorRealOperations.

Open Scope ring_scope.

Context {R : realType}.
Context {ds : list nat}.
Notation zipWithR := (@zipWith R R R ds) (only parsing).
Notation mapR := (@map R R ds) (only parsing).

Definition addt := zipWithR GRing.add.
Definition subt := zipWithR (fun x y => GRing.add x (GRing.opp y)).
Definition mult := zipWithR GRing.mul.
Definition oppt := mapR GRing.opp.
Definition maxt := zipWithR Order.max.
Definition mint := zipWithR Order.min.
Definition invt := mapR GRing.inv.

Global Coercion tensor_of_real := @tensor_of R.

Definition reduceAdd {ds} : tensor R ds -> R := reduce GRing.add 0.
Definition reduceMul {ds} : tensor R ds -> R := reduce GRing.mul 1.

End TensorRealOperations.

Notation "x +%t y" := (addt x y) (at level 50) : tensor_scope.
Notation "x *%t y" := (mult x y) (at level 40) : tensor_scope.
Notation "-%t x" := (oppt x) (at level 35): tensor_scope.
Notation "x -%t y" := (addt x (oppt y)) (at level 50) : tensor_scope.
Notation "x ^-1%t" := (invt x) : tensor_scope.
Notation "x /%t y" := (mult x (invt y)) (at level 40) : tensor_scope.
