From mathcomp Require Import ssreflect ssrfun seq eqtype fintype ssrbool order.
From mathcomp Require Import matrix tuple reals.
From vehicle Require Import tensor.
Open Scope ring_scope.
(* Vehicle standard library definitions *)

Definition forallInList {A} (f : A -> Prop) (s : seq A) := foldr and True (map f s).

Definition existsInList {A} (f : A -> Prop) (s : seq A) := foldr or False (map f s).

Definition existsIndex {n} (f : 'I_n -> Prop) := exists x, f x.

Definition forallIndex {n} (f : 'I_n -> Prop) := forall x, f x.

Definition foreachTuple {n A} (f : 'I_n -> A) := mktuple f.

Definition reduceAnd {us ds} (t : 'T[bool]_(us, ds)) : bool :=
    [forall ij, \val t ij.1 ij.2].

Definition reduceOr {us ds} (t : 'T[bool]_(us, ds)) : bool :=
    [exists ij, \val t ij.1 ij.2].

Section Tensor.

Open Scope order_scope.

Context {R : realType} {us ds : pseq}.

Notation reduceAndMap r := [rel xs ys | reduceAnd (@map2_t R R bool r us ds xs ys)].

Definition eqRatTensorReduced := reduceAndMap [rel x y | x == y].
Definition neRatTensorReduced := reduceAndMap [rel x y | x != y].
Definition leRatTensorReduced := reduceAndMap [rel x y | x <= y].
Definition ltRatTensorReduced := reduceAndMap [rel x y | x < y].
Definition geRatTensorReduced := reduceAndMap [rel x y | x >= y].
Definition gtRatTensorReduced := reduceAndMap [rel x y | x > y].

End Tensor.
