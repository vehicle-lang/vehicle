From mathcomp Require Import seq fintype eqtype reals ssrbool order.
From vehicle Require Import tensor.
Open Scope ring_scope.
(* Vehicle standard library definitions *)

Definition forallInList {A} (f : A -> Prop) (s : seq A):= foldr and True (map f s).

Definition existsInList {A} (f : A -> Prop) (s : seq A) := foldr or False (map f s).

Definition existsIndex {n} (f : 'I_n -> Prop) := exists x, f x.

Definition forallIndex {n} (f : 'I_n -> Prop) := forall x, f x.

Section Tensor.

Context {R : realType}.
Context {ds : seq nat}.

Definition eqRatTensorReduced (xs ys : tensor R ds) := reduceAnd (zipWith eq_op xs ys).
Definition neRatTensorReduced (xs ys : tensor R ds) :=  ~~ (eqRatTensorReduced xs ys).
Definition leRatTensorReduced (xs ys : tensor R ds) := reduceAnd (zipWith Order.le xs ys).
Definition ltRatTensorReduced (xs ys : tensor R ds) := reduceAnd (zipWith Order.lt xs ys).
Definition geRatTensorReduced (xs ys : tensor R ds) := reduceAnd (zipWith Order.ge xs ys).
Definition gtRatTensorReduced (xs ys : tensor R ds) := reduceAnd (zipWith Order.gt xs ys).

End Tensor.
