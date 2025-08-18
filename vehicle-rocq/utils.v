From mathcomp Require Import ssreflect seq fintype eqtype reals ssrbool order bigop ssrfun matrix.
From vehicle Require Import tensor.
Open Scope ring_scope.
Open Scope big_scope.
(* Vehicle standard library definitions *)

Definition forallInList {A} (f : A -> Prop) (s : seq A):= foldr and True (map f s).

Definition existsInList {A} (f : A -> Prop) (s : seq A) := foldr or False (map f s).

Definition existsIndex {n} (f : 'I_n -> Prop) := exists x, f x.

Definition forallIndex {n} (f : 'I_n -> Prop) := forall x, f x.

Definition map2_tensor {X Y Z us ds} (f : X -> Y -> Z) (t : 'T[X]_(us, ds)) (v : 'T[Y]_(us, ds)) :=
    @Tensor us ds Z (map2_mx f (\val t) (\val v)).

Definition reduceAnd {us ds} (t : 'T[bool]_(us, ds)) : bool :=
    [forall ij, \val t ij.1 ij.2].

Section Tensor.

Open Scope order_scope.

Context {R : realType} {us ds : seq.+1}.

Notation reduceAndMap r := [rel xs ys | reduceAnd (@map2_tensor R R bool us ds r xs ys)].

Definition eqRatTensorReduced := reduceAndMap [rel x y | x == y].
Definition neRatTensorReduced := reduceAndMap [rel x y | x != y].
Definition leRatTensorReduced := reduceAndMap [rel x y | x <= y].
Definition ltRatTensorReduced := reduceAndMap [rel x y | x < y].
Definition geRatTensorReduced := reduceAndMap [rel x y | x >= y].
Definition gtRatTensorReduced := reduceAndMap [rel x y | x > y].

End Tensor.
