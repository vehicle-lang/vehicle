From HB Require Import structures.
From mathcomp Require Import ssreflect ssrfun seq eqtype choice ssrnat ssralg.
From mathcomp Require Import fintype ssrbool order matrix bigop tuple finfun.
From mathcomp Require Import reals interval_inference numdomain tensor.
Local Open Scope ring_scope.
(* Vehicle standard library definitions *)
Notation "t ^^ i" := (nindex t i) (at level 30, i at level 30, format "t ^^ i").
Notation "t .[::]" := (tensor_nil t).

(* Pointwise order on tensors: t <= u iff t_ij <= u_ij for all i, j.
   Sharing R's display so order_scope notations apply directly. *)
Section TensorPOrder.

Import Order.POrderTheory.
Local Open Scope order_scope.

Context (d : Order.disp_t) (R : porderType d).
Context {l k : nat} (u_ : {posnum nat} ^ k) (d_ : {posnum nat} ^ l).

Definition le_t (t u : 'T[R]_(u_, d_)) :=
  [forall ij, (\val t ij.1 ij.2) <= (\val u ij.1 ij.2)].

Definition lt_t (t u : 'T[R]_(u_, d_)) := (u != t) && le_t t u.

Let lt_t_def : forall x y, lt_t x y = (y != x) && le_t x y.
Proof. by []. Qed.

Let le_t_refl : reflexive le_t.
Proof. by move=> x; exact /forallP. Qed.

Let le_t_anti : antisymmetric le_t.
Proof.
move=> x y /andP[/forallP le_t_xy /forallP le_t_yx].
apply/val_inj/matrixP=> i j; apply /le_anti/andP.
exact (conj (le_t_xy (i, j)) (le_t_yx (i, j))).
Qed.

Let le_t_trans : transitive le_t.
Proof.
move=> x y z /forallP le_t_yx /forallP le_t_xz.
apply/forallP=> ij; exact /le_trans.
Qed.

HB.instance Definition _ := Order.isPOrder.Build
  d 'T[R]_(u_, d_) lt_t_def le_t_refl le_t_anti le_t_trans.

End TensorPOrder.

Section NilTensorOrderTheory.

Local Open Scope order_scope.
Import Order.POrderTheory.

Lemma tensor_nil_leP {d : Order.disp_t} {R : porderType d} t u
  : t.[::] <= u.[::] :> R <-> t <= u.
Proof.
split=> [?|/forallP/(_ (cast_ord prod_nil ord0, cast_ord prod_nil ord0))//].
by apply/forallP=> [[] i j]; rewrite !ord_prod_nil.
Qed.

Lemma tensor_nil_ltP {d : Order.disp_t} {R : porderType d} t u
  : t.[::] < u.[::] :> R <-> t < u.
Proof.
rewrite !lt_def; split=> /andP [u_neq_t t_le_u]; apply/andP; split.
      by apply/contra_neq; first apply (tensor_nil_eqP u t).
    by apply/tensor_nil_leP.
  by apply/contra_neq; first apply (tensor_nil_eqP u t).
by apply/tensor_nil_leP.
Qed.

End NilTensorOrderTheory.

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

Local Open Scope order_scope.

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

(* -------------------------------------------------------------------------- *)
(* Staging area for tensor definitions intended to be upstreamed to           *)
(* mathcomp's tensor library. Until then they live here so that the           *)
(* Vehicle-to-Rocq backend can refer to them.                                 *)
(* -------------------------------------------------------------------------- *)

Section TensorTranspose.

(* 2-D transpose only. The general n-D case needs a tabulate-and-lookup
   primitive on tensors that mathcomp's tensor library does not yet expose.
   The dimensions are kept as `{posnum nat}` (rather than `_.+1`) so the type
   unifies directly with the literal posnat dimensions emitted by the backend. *)
Context {R : Type} {n m : {posnum nat}}.

Definition transpose_t (t : 'nT[R]_([tuple n; m])) : 'nT[R]_([tuple m; n]) :=
  nstack (fun j => nstack (fun i => (t ^^ i) ^^ j)).

(* Correctness: transpose_t swaps the two axes,
   i.e. (transpose_t t)[j][i] = t[i][j]. *)
Lemma transpose_tE (t : 'nT[R]_([tuple n; m])) i j :
  ((transpose_t t) ^^ j) ^^ i = (t ^^ i) ^^ j.
Proof. by rewrite /transpose_t !nstackE. Qed.

End TensorTranspose.
