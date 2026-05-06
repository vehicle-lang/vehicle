From mathcomp Require Import ssreflect ssrfun seq eqtype fintype ssrbool order.
From mathcomp Require Import matrix tuple reals finfun.
From mathcomp Require Import interval_inference numdomain tensor.
Open Scope ring_scope.
(* Vehicle standard library definitions *)

Definition forallInList {A} (f : A -> Prop) (s : seq A) := foldr and True (map f s).

Definition existsInList {A} (f : A -> Prop) (s : seq A) := foldr or False (map f s).

Definition existsIndex {n} (f : 'I_n -> Prop) := exists x, f x.

Definition forallIndex {n} (f : 'I_n -> Prop) := forall x, f x.

Definition foreachTuple {n A} (f : 'I_n -> A) := mktuple f.

Definition reduceAnd {k l} {u_ : {posnum nat} ^ k} {d_ : {posnum nat} ^ l}
    (t : 'T[bool]_(u_, d_)) : bool :=
  [forall ij, \val t ij.1 ij.2].

Definition reduceOr {k l} {u_ : {posnum nat} ^ k} {d_ : {posnum nat} ^ l}
    (t : 'T[bool]_(u_, d_)) : bool :=
  [exists ij, \val t ij.1 ij.2].

Definition map2_t {R S T : Type} (f : R -> S -> T)
    {k l} {u_ : {posnum nat} ^ k} {d_ : {posnum nat} ^ l}
    (t : 'T[R]_(u_, d_)) (v : 'T[S]_(u_, d_)) : 'T[T]_(u_, d_) :=
  Tensor (map2_mx f (\val t) (\val v)).

Section Tensor.

Open Scope order_scope.

Context {R : realType} {k l : nat}
  {u_ : {posnum nat} ^ k} {d_ : {posnum nat} ^ l}.

Notation reduceAndMap r := [rel xs ys | reduceAnd (@map2_t R R bool r k l u_ d_ xs ys)].

Definition eqRatTensorReduced := reduceAndMap [rel x y | x == y].
Definition neRatTensorReduced := reduceAndMap [rel x y | x != y].
Definition leRatTensorReduced := reduceAndMap [rel x y | x <= y].
Definition ltRatTensorReduced := reduceAndMap [rel x y | x < y].
Definition geRatTensorReduced := reduceAndMap [rel x y | x >= y].
Definition gtRatTensorReduced := reduceAndMap [rel x y | x > y].

End Tensor.
