From HB Require Import structures.
From mathcomp Require Import ssreflect seq matrix bigop ssrbool eqtype choice.
From mathcomp Require Import fintype ssralg ssrnat ssrfun order finfun tuple.

(******************************************************************************)
(* This file defines tensors,                                                 *)
(* For tensors we define:                                                     *)
(*                 pseq == the type of sequences of strictly positive natural *)
(*                         numbers, coerces to and from seq.                  *)
(*       'T[R]_(us, ds) == the type of tensors with elements of type R,       *)
(*       'T_(us, ds)       contravariant dimensions us, and covariant         *)
(*                         dimensions ds, e.g. 'T[nat]_([:: 1; 3], [::])      *)
(*                         (us and ds must be instances of pseq).             *)
(*                         The [R] is optional and can usually be ommited.    *)
(* 'nT[R]_(us), 'nT_(us) == 'T[R]_(us, [::]), purely contravariant tensors.   *)
(* 'oT[R]_(ds), 'oT_(ds) == 'T[R]_([::], ds), purely covariant tensors.       *)
(*                  t^^i == the tensor obtained by fixing the first           *)
(*                          contravariant dimension of t to i.                *)
(*                  t`_i == the tensor obtained by fixing the first           *)
(*                          covariant dimension of t to i.                    *)
(* \tensor^^(i < n) Expr(i) ==                                                *)
(*                         the 'T_(n :: _, _) tensor t such that              *)
(*                         t^^i = Expr(i) with i : 'I_n, the < n bound can    *)
(*                         usually be ommitted.                               *)
(* \tensor`_(j < n) Expr(j) ==                                                *)
(*                         the 'T_(_, n :: _) tensor t such that              *)
(*                         t`_j = Expr(j) with j : 'I_n, the < n bound can    *)
(*                         usually be ommitted.                               *)
(*            const_t v == the constant tensor whose entries are all v        *)
(*                         (dimensions are inferred from context)             *)
(*               t.[::] == the value of the single entry in a                 *)
(*                         tensor t : 'T_([::], [::]).                        *)
(*         t^^=i, t`_=i == variants of the indexing operations equivalent to  *)
(*                         (t^^i).[::], (t`_i).[::], useful for indexing the  *)
(*                         final dimnsions of tensors.                        *)
(* \tensor^^(i < n) => Expr(i), \tensor`_(j < n) => Expr(j) ==                *)
(*                         variant constructor equivalent to                  *)
(*                         \tensor^^(i < n) const_t Expr(i).                  *)
(* [tensor^^ t_0; ...; t_n], [tensor`_ t_0; ...; t_n] ==                      *)
(*                         construction of a tensor from a sequence of        *)
(*                         tensors.                                           *)
(* [tensor^^= x_0; ...; x_n], [tensor`_= x_0; ...; x_n] ==                    *)
(*                         construction of a tensor from a sequence of values.*)
(* nunstack_tuple, ounstack_tuple ==                                          *)
(*                         conversion from tensors of types 'T_(n :: us, ds)  *)
(*                         and 'T_(us, n :: ds) respectively, to              *)
(*                         n.-tuple 'T_(us, ds).                              *)
(* tuple_of_ntensor, tuple_of_otensor ==                                      *)
(*                         conversion between tensors of types 'nT_([:: n])   *)
(*                         and 'oT_([:: n]) respectively to n tuples.         *)
(* tensor_of_matrix, matrix_of_tensor ==                                      *)
(*                         bijection between n by m matricies and tensors of  *)
(*                         type 'T_([:: n], [:: m]).                          *)
(* Tensors are instances of subType of matrix, and inherit all combinatorial  *)
(* structures (including finType), as well as nmodType, zmodType,             *)
(* lSemiModType, lModType.                                                    *)
(*   Tensors also implement a pointwise partial ordering, as well as          *)
(* ring instances where the underlying type satisfies that instance.          *)
(******************************************************************************)

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.


Reserved Notation "''nT_' ( us )"
  (at level 0, us at level 2, format "''nT_' ( us )").
Reserved Notation "''oT_' ( ds )"
  (at level 0, ds at level 2, format "''oT_' ( ds )").
Reserved Notation "''nT[' R ]_ ( us )" (at level 0, us at level 2).
  (* only parsing *)
Reserved Notation "''oT[' R ]_ ( ds )" (at level 0, ds at level 2).
  (* only parsing *)
Reserved Notation "''T_' ( us , ds )"
  (at level 0, us at level 2, ds at level 2, format "''T_' ( us ,  ds )").
Reserved Notation "''T[' R ]_ ( us , ds )"
  (at level 0, us at level 2, ds at level 2). (* only parsing*)

Reserved Notation "t ^^ i"
  (at level 3, i at level 2, left associativity, format "t ^^ i").
Reserved Notation "t `_ i"
  (at level 3, i at level 2, left associativity, format "t `_ i").
Reserved Notation "t ^^= i"
  (at level 3, i at level 2, left associativity, format "t ^^= i").
Reserved Notation "t `_= i"
  (at level 3, i at level 2, left associativity, format "t `_= i").

Reserved Notation "\tensor ^^ i E"
  (at level 34, E at level 39, i binder, format "\tensor ^^ i  E").
Reserved Notation "\tensor `_ i E"
  (at level 34, E at level 39, i binder, format "\tensor `_ i  E").
Reserved Notation "\tensor ^^ ( i < u ) E"
  (E at level 39, i, u at level 50). (* only parsing *)
Reserved Notation "\tensor `_ ( i < d ) E"
  (E at level 39, i, d at level 50). (* only parsing *)
Reserved Notation "\tensor ^^ i => E"
  (at level 34, E at level 39, i binder, format "\tensor ^^ i  =>  E").
Reserved Notation "\tensor `_ i => E"
  (at level 34, E at level 39, i binder, format "\tensor `_ i  =>  E").
Reserved Notation "\tensor ^^ ( i < u ) => E"
  (E at level 39, i, u at level 50). (* only parsing *)
Reserved Notation "\tensor `_ ( i < d ) => E"
  (E at level 39, i, d at level 50). (* only parsing *)

Reserved Notation "[ 'tensor' ^^ t ; .. ; tn ]"
  (format "[ 'tensor' ^^ '['  t ; '/'  .. ; '/'  tn ']' ]").
Reserved Notation "[ 'tensor' `_ t ; .. ; tn ]"
  (format "[ 'tensor' `_ '['  t ; '/'  .. ; '/'  tn ']' ]").
Reserved Notation "[ 'tensor' ^^= x ; .. ; xn ]"
  (format "[ 'tensor' ^^= '['  t ; '/'  .. ; '/'  tn ']' ]").
Reserved Notation "[ 'tensor' `_= x ; .. ; xn ]"
  (format "[ 'tensor' `_= '['  t ; '/'  .. ; '/'  tn ']' ]").

Reserved Notation "t .[::]".


Structure pseq : Type := PSeq
  { psval :> seq nat
  ; _ : all [pred x | x != 0] psval
  }.

HB.instance Definition _ := [isSub for psval].

Canonical nil_pseq := PSeq (isT : all [pred x | x != 0] [::]).
Canonical cons_pseq p (ps : pseq) :=
  PSeq (valP ps : all [pred x | x != 0] (p.+1 :: ps)).


Section TensorDef.

Context (R : Type) (us ds : pseq).

Variant tensor_of : Type :=
  Tensor of 'M[R]_(\prod_(u <- us) u, \prod_(d <- ds) d).

Definition tval t := let: Tensor g := t in g.

HB.instance Definition _ := [isNew for tval].

End TensorDef.


Fact tensor_key : unit. Proof. by []. Qed.

HB.lock
Definition tensor_of_mx R (us ds : pseq) (k : unit) M :=
  @Tensor R us ds M.
Canonical tensor_unlockable := Unlockable tensor_of_mx.unlock.


Notation "''T[' R ]_ ( us , ds )" := (tensor_of R us ds) (only parsing).
Notation "''T_' ( us , ds )" := 'T[_]_(us, ds).
Notation "''nT[' R ]_ ( us )" := 'T[R]_(us, [::]) (only parsing).
Notation "''oT[' R ]_ ( ds )" := 'T[R]_([::], ds) (only parsing).
Notation "''oT_' ( ds )" := 'oT[_]_(ds).
Notation "''nT_' ( us )" := 'nT[_]_(us).


Section SubtypeInstances.

Context (us ds : pseq).
Local Notation "''T[' R ]" := 'T[R]_(us, ds).

HB.instance Definition _ (R : eqType) := [Equality of 'T[R] by <:].
HB.instance Definition _ (R : choiceType) := [Choice of 'T[R] by <:].
HB.instance Definition _ (R : countType) := [Countable of 'T[R] by <:].
HB.instance Definition _ (R : finType) := [Finite of 'T[R] by <:].

Lemma nmod_closed {m n} (R : nmodType) : @addr_closed 'M[R]_(n, m) predT.
Proof. by []. Qed.
HB.instance Definition _ (R : nmodType) := GRing.SubChoice_isSubNmodule.Build
  _ _ 'T[R] (nmod_closed R).

(* REPLACE IN NEXT MATHCOMP RELEASE *)
(* Lemma zmod_closed {m n} (R : zmodType) : @zmod_closed 'M[R]_(n, m) predT.
Proof. by []. Qed.
HB.instance Definition _ (R : zmodType) := GRing.SubChoice_isSubZmodule.Build
  _ _ 'T[R] (zmod_closed R).

Lemma subsemimod_closed {m n} (R : pzSemiRingType)
  : @subsemimod_closed R 'M[R]_(n, m) predT.
Proof. by []. Qed.
HB.instance Definition _ (R : pzSemiRingType) :=
  GRing.SubNmodule_isSubLSemiModule.Build _ _ _ 'T[R] (subsemimod_closed R).

Lemma submod_closed {m n} (R : pzRingType) : @submod_closed R 'M[R]_(n, m) predT.
Proof. by []. Qed.
HB.instance Definition _ (R : pzRingType) :=
  GRing.SubZmodule_isSubLmodule.Build _ _ _ 'T[R] (submod_closed R). *)

End SubtypeInstances.


Section TensorPOrder.

Import Order.POrderTheory.
Open Scope order_scope.

Context (o : Order.disp_t) (R : porderType o) (us ds : pseq).

Definition le_t (t u : 'T[R]_(us, ds)) :=
  [forall ij, (\val t ij.1 ij.2) <= (\val u ij.1 ij.2)].

Definition lt_t (t u : 'T[R]_(us, ds)) := (u != t) && le_t t u.

Lemma lt_t_def : forall x y, lt_t x y = (y != x) && le_t x y.
Proof. by []. Qed.

Lemma le_t_refl : reflexive (le_t).
Proof. by move=> x; exact /forallP. Qed.

Lemma le_t_anti : antisymmetric (le_t).
Proof.
move=> x y /andP[/forallP le_t_xy /forallP le_t_yx].
apply/val_inj/matrixP=> i j; apply /le_anti/andP.
exact (conj (le_t_xy (i, j)) (le_t_yx (i, j))).
Qed.

Lemma le_t_trans : transitive (le_t).
Proof.
move=> x y z /forallP le_t_yx /forallP le_t_xz.
apply/forallP=> ij; exact /le_trans.
Qed.

HB.instance Definition _ := Order.isPOrder.Build
  o 'T[R]_(us, ds) lt_t_def le_t_refl le_t_anti le_t_trans.

End TensorPOrder.


Definition const_t {R us ds} v : 'T[R]_(us, ds) :=
  tensor_of_mx tensor_key (const_mx v).


Section MapTensor.

Variables (aT rT : Type) (f : aT -> rT).

Definition map_t us ds (t : 'T[aT]_(us, ds)) :=
  tensor_of_mx tensor_key (map_mx f (\val t)).

End MapTensor.

Section Map2Tensor.

Context {R S T : Type} (f : R -> S -> T).

Definition map2_t us ds (t : 'T[R]_(us, ds)) (v : 'T[S]_(us, ds)) :=
  tensor_of_mx tensor_key (map2_mx f (\val t) (\val v)).

End Map2Tensor.


Section TensorLaws.

Context {T : Type} {us ds : pseq} {idm : T}.

Lemma map2_tA {opm : Monoid.law idm}
  : associative (@map2_t _ _ _ opm us ds).
Proof. by rewrite/map2_t unlock=> t u v; apply/congr1/map2_mxA. Qed.

Lemma map2_1t {opm : Monoid.law idm} :
  left_id (const_t idm) (@map2_t _ _ _ opm us ds).
Proof. by rewrite /map2_t/const_t unlock=> [[M]]; apply/congr1/map2_1mx. Qed.

Lemma map2_t1 {opm : Monoid.law idm} :
  right_id (const_t idm) (@map2_t _ _ _ opm us ds).
Proof. by rewrite /map2_t/const_t unlock=> [[M]]; apply/congr1/map2_mx1. Qed.

HB.instance Definition _ {opm : Monoid.law idm} :=
  Monoid.isLaw.Build 'T_(us, ds) (const_t idm) (@map2_t _ _ _ opm _ _)
    map2_tA map2_1t map2_t1.

Lemma map2_tC {opm : Monoid.com_law idm} :
  commutative (@map2_t _ _ _ opm us ds).
Proof. by rewrite /map2_t unlock=> t u; apply/congr1/map2_mxC. Qed.

HB.instance Definition _ {opm : Monoid.com_law idm} :=
  SemiGroup.isCommutativeLaw.Build
  'T_(us, ds) (@map2_t _ _ _ opm _ _) map2_tC.

End TensorLaws.



Section TensorRing.

Open Scope ring_scope.
Import GRing.Theory.

Context {us ds : pseq}.
Local Notation "''T[' R ]" := 'T[R]_(us, ds).

(* REPLACE IN NEXT MATHCOMP RELEASE *)
Section TensorZmodule.

Context {R : zmodType}.

Definition oppt (t : 'T[R]) := tensor_of_mx tensor_key (- \val t).

Lemma addNt : left_inverse 0 oppt +%R.
Proof.
rewrite /oppt unlock=> t; apply/val_inj/matrixP=> i j.
by rewrite /GRing.opp/=/GRing.add/= addNmx.
Qed.

HB.instance Definition _ := GRing.Nmodule_isZmodule.Build 'T[R] addNt.

End TensorZmodule.

Section TensorSemiRing.

Context {R : pzSemiRingType}.

Definition tensor1 := @const_t _ us ds (GRing.one R).

Definition mult := @map2_t R R R *%R us ds.

Definition multDl : left_distributive mult +%R.
Proof. by rewrite /mult/map2_t unlock=> t u v; apply/congr1/map2_mxDl. Qed.
Lemma multDr : right_distributive mult +%R.
Proof. by rewrite /mult/map2_t unlock=> t u v; apply/congr1/map2_mxDr. Qed.
Definition mul0t : left_zero 0%R mult.
Proof. by rewrite /mult/map2_t unlock=> t; apply/congr1/map2_0mx. Qed.
Lemma mult0 : right_zero 0%R mult.
Proof. by rewrite /mult/map2_t unlock=> t; apply/congr1/map2_mx0. Qed.

HB.instance Definition _ := GRing.Nmodule_isPzSemiRing.Build
  'T[R] map2_tA map2_1t map2_t1 multDl multDr mul0t mult0.

End TensorSemiRing.

HB.instance Definition _ {R : pzRingType} := GRing.Zmodule_isPzRing.Build
  'T[R] map2_tA map2_1t map2_t1 multDl multDr.

HB.instance Definition _ {R : comPzRingType} :=
  GRing.PzRing_hasCommutativeMul.Build 'T[R] map2_tC.

Lemma onet_neq0 {R : nzSemiRingType} : 1%R != 0%R :> 'T[R].
Proof.
rewrite /GRing.one/GRing.zero/=/const_t unlock.
apply/eqP; case; apply/matrixP.
have prod_pseq_neq0: forall xs : pseq, \prod_(e <- xs) e != 0=> [xs|].
  by rewrite prod_nat_seq_neq0; case xs.
case: (\prod_(u <- _) u) (prod_pseq_neq0 us)=> [//|n0 _] /(_ ord0).
case: (\prod_(d <- _) d) (prod_pseq_neq0 ds)=> [//|n1 _] /(_ ord0).
by rewrite !mxE; apply/eqP/oner_neq0.
Qed.

HB.instance Definition _ {R : nzSemiRingType} :=
  GRing.PzSemiRing_isNonZero.Build
  'T[R] onet_neq0.

HB.instance Definition _ {R : nzRingType} := GRing.Zmodule_isNzRing.Build
  'T[R] map2_tA map2_1t map2_t1 multDl multDr onet_neq0.

HB.instance Definition _ {R : comNzRingType} := GRing.Zmodule_isComNzRing.Build
  'T[R] map2_tA map2_tC map2_1t multDl onet_neq0.

Definition unitt {R : unitRingType} (t : 'T[R]) :=
  [forall ij, (\val t ij.1 ij.2) \is a GRing.unit].

Definition invt {R : unitRingType} (t : 'T[R]) :=
  if t \in unitt then Tensor (map_mx GRing.inv (\val t)) else t.

Definition mulVt {R : unitRingType} : {in @unitt R, left_inverse 1%R invt *%R}.
Proof.
move=> t t_unit; apply/val_inj/matrixP=> i j.
rewrite /invt t_unit/GRing.mul/GRing.one/=/map2_t/const_t.
rewrite tensor_of_mx.unlock !mxE mulVr//.
by move: t_unit; rewrite /unitt=> /forallP /(_ (i, j)).
Qed.

Definition divtt {R : unitRingType} : {in @unitt R, right_inverse 1%R invt *%R}.
Proof.
move=> t t_unit; apply/val_inj/matrixP=> i j/=.
rewrite /invt t_unit /GRing.mul/GRing.one/=/map2_t/const_t.
rewrite tensor_of_mx.unlock !mxE divrr//.
by move: t_unit; rewrite /unitt=> /forallP /(_ (i, j)).
Qed.

Definition unittP {R : unitRingType}
  : forall x y, y * x = 1%R /\ x * y = 1 -> @unitt R x.
Proof.
move=> x y [/eqP + /eqP]; rewrite /eq_op/==> /eqP/matrixP yx1 /eqP/matrixP xy1.
apply/forallP=> ij; apply/unitrP; exists (\val y ij.1 ij.2).
move: (conj (yx1 ij.1 ij.2) (xy1 ij.1 ij.2)).
rewrite {1 2}/GRing.mul{1 2}/GRing.one/=/map2_t/const_t.
by rewrite tensor_of_mx.unlock !mxE.
Qed.

Definition invt_out {R : unitRingType} : {in [predC @unitt R], invt =1 id}.
Proof. by move=> t /negbTE t_not_unit; rewrite /invt t_not_unit. Qed.

HB.instance Definition _ {R : unitRingType} :=
  GRing.NzRing_hasMulInverse.Build 'T[R] mulVt divtt unittP invt_out.

End TensorRing.


Section NilTensor.

Context (R : Type).

Lemma prod_nil : 1 = \prod_(e <- [::]) e.
Proof. by rewrite big_nil. Qed.

Lemma ord_prod_nil : all_equal_to (cast_ord prod_nil ord0).
Proof.
case=> [[?|n n_ord]]; apply: val_inj=>//=.
by move: n_ord; rewrite -prod_nil.
Qed.

Definition tensor_nil (t : 'T[R]_([::], [::])) : R :=
  \val t (cast_ord prod_nil ord0) (cast_ord prod_nil ord0).

Definition const_tK : cancel const_t tensor_nil.
Proof. by rewrite /const_t unlock /tensor_nil=> t; rewrite mxE. Qed.

Definition tensor_nilK : cancel tensor_nil const_t.
Proof.
rewrite /const_t unlock=> t; apply/val_inj/matrixP=> i j.
by rewrite mxE !ord_prod_nil.
Qed.

End NilTensor.


Notation "t .[::]" := (tensor_nil t).


Section NilTensorTheory.

Lemma tensor_nil_eqP {R : Type} t u : t.[::] = u.[::] :> R <-> t = u.
Proof.
by split=> [?|->//]; apply/val_inj/matrixP=> i j; rewrite !ord_prod_nil.
Qed.

Open Scope order_scope.
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

Close Scope order_scope.
Open Scope ring_scope.
Import GRing.Theory.

Lemma tensor_nilD {R : nmodType} t u
  : (t + u).[::] = t.[::] + u.[::] :> R.
Proof. by rewrite /tensor_nil mxE. Qed.

Lemma tensor_nilN {R : zmodType} t
  : (- t).[::] = - t.[::] :> R.
Proof. by rewrite {1}/GRing.opp/=/oppt unlock /tensor_nil mxE. Qed.

Lemma tensor_nilM {R : pzSemiRingType} t u
  : (t * u).[::] = t.[::] * u.[::] :> R.
Proof. by rewrite {1}/GRing.mul/=/map2_t unlock /tensor_nil mxE. Qed.

Definition tensor_nilr_spec {R : pzRingType} :=
  (@tensor_nilM R, @tensor_nilN R, @tensor_nilD R).

Lemma tensor_nilV {R : unitRingType} t
  : (t^-1).[::] = t.[::]^-1 :> R.
Proof.
rewrite {1}/GRing.inv/=/invt.
case t_unit: (t \in _); first by rewrite /tensor_nil mxE.
apply/esym/invr_out/negP=> ?; move: t_unit=> /negbT /forallP.
by apply=>>; rewrite !ord_prod_nil.
Qed.

End NilTensorTheory.


Section ConstTensorTheory.

Context (us ds : pseq).

Open Scope ring_scope.
Import GRing.Theory.

Lemma const_tD {R : nmodType} x y
  : @const_t R us ds (x + y) = const_t x + const_t y.
Proof.
by rewrite /const_t unlock; apply/val_inj/matrixP=> i j; rewrite !mxE.
Qed.

Lemma const_tN {R : zmodType} x
  : @const_t R us ds (- x) = - const_t x.
Proof.
rewrite {2}/GRing.opp/=/oppt/const_t unlock.
by apply/val_inj/matrixP=> i j; rewrite !mxE.
Qed.

Lemma const_tM {R : pzSemiRingType} x y
  : @const_t R us ds (x * y) = const_t x * const_t y.
Proof.
rewrite {2}/GRing.mul/=/mult/const_t/map2_t unlock.
by apply/val_inj/matrixP=> i j; rewrite !mxE.
Qed.

Definition const_tr_spec {R : pzRingType} :=
  (@const_tM R, @const_tN R, @const_tD R).

Lemma const_tV {R : unitRingType} x
  : @const_t R us ds x^-1 = (const_t x)^-1.
Proof.
rewrite /const_t unlock.
apply/val_inj/matrixP=> i j.
rewrite {2}/GRing.inv/=/invt.
case t_unit: (Tensor _ \in _); rewrite !mxE=>//.
apply/invr_out/negP=> ?; move: t_unit=> /negbT /forallP.
by apply=>>; rewrite mxE.
Qed.

End ConstTensorTheory.


Section IndexTensor.

Section IndexTensorBij.

Context {x : nat} {xs : seq nat}.

Lemma tensormx_cast
: #|{:'I_x * 'I_\prod_(e <- xs) e}| = \prod_(e <- x :: xs) e.
Proof. by rewrite card_prod !card_ord big_cons. Qed.

Definition tensormx_index (ij : 'I_x * 'I_\prod_(e <- xs) e)
  : 'I_\prod_(e <- x :: xs) e :=
  cast_ord tensormx_cast (enum_rank ij).

Definition tensormx_unindex (i : 'I_\prod_(e <- x :: xs) e)
  : 'I_x * 'I_\prod_(e <- xs) e :=
  enum_val (cast_ord (esym tensormx_cast) i).

Lemma tensormx_indexK : cancel tensormx_index tensormx_unindex.
Proof. by move=> ij; rewrite /tensormx_unindex cast_ordK enum_rankK. Qed.

Lemma tensormx_unindexK : cancel tensormx_unindex tensormx_index.
Proof. by move=> i; rewrite /tensormx_index enum_valK cast_ordKV. Qed.

End IndexTensorBij.

Context (R : Type) (u d : nat) (us ds : pseq).

Open Scope ring_scope.

Definition nindex (t : 'T[R]_(u.+1 :: us, ds)) (i : 'I_u.+1) : 'T[R]_(us, ds) :=
  tensor_of_mx tensor_key (\matrix_(i', j) (\val t) (tensormx_index (i, i')) j).

Definition oindex (t : 'T[R]_(us, d.+1 :: ds)) (j : 'I_d.+1) : 'T[R]_(us, ds) :=
  tensor_of_mx tensor_key (\matrix_(i, j') (\val t) i (tensormx_index (j, j'))).

Definition nstack (f : 'I_u.+1 -> 'T[R]_(us, ds)) : 'T[R]_(u.+1 :: us, ds) :=
  tensor_of_mx tensor_key (
    \matrix_(i, j) \val (f (tensormx_unindex i).1) (tensormx_unindex i).2 j).

Definition ostack (f : 'I_d.+1 -> 'T[R]_(us, ds)) : 'T[R]_(us, d.+1 :: ds) :=
  tensor_of_mx tensor_key (
    \matrix_(i, j) \val (f (tensormx_unindex j).1) i (tensormx_unindex j).2).

End IndexTensor.


Notation "t ^^ i" := (nindex t i).
Notation "t `_ i" := (oindex t i).
Notation "t ^^= i" := ((t^^i).[::]).
Notation "t `_= i" := ((t`_i).[::]).

Notation "\tensor ^^ ( i < u ) E" := (nstack (fun i : 'I_u => E))
  (only parsing).
Notation "\tensor `_ ( i < d ) E" := (ostack (fun i : 'I_d => E))
  (only parsing).
Notation "\tensor ^^ i E" := (\tensor^^(i < _) E).
Notation "\tensor `_ i E" := (\tensor`_(i < _) E).
Notation "\tensor ^^ ( i < u ) => E" := (\tensor^^(i < u) const_t E)
  (only parsing).
Notation "\tensor `_ ( i < d ) => E" := (\tensor`_(i < d) const_t E)
  (only parsing).
Notation "\tensor ^^ i => E" := (\tensor^^i const_t E).
Notation "\tensor `_ i => E" := (\tensor`_i const_t E).


Section TensorIndexTheory.

Context (R : Type).

Lemma ntensorP {u} {us ds : pseq} (t v : 'T[R]_(u.+1 :: us, ds))
  : t = v <-> forall i, t^^i = v^^i.
Proof.
rewrite /nindex unlock.
split=> [->//|eq_i]; apply/val_inj/matrixP=> i j.
move: (eq_i (tensormx_unindex i).1)=> [/matrixP] /(_ (tensormx_unindex i).2 j).
by rewrite !mxE -surjective_pairing tensormx_unindexK.
Qed.

Lemma otensorP {d} {us ds : pseq} (t v : 'T[R]_(us, d.+1 :: ds))
  : t = v <-> forall i, t`_i = v`_i.
Proof.
rewrite /oindex unlock.
split=> [->//|eq_i]; apply/val_inj/matrixP=> i j.
move: (eq_i (tensormx_unindex j).1)=> [/matrixP] /(_ i (tensormx_unindex j).2).
by rewrite !mxE -surjective_pairing tensormx_unindexK.
Qed.

Lemma ntensor_eqP {u} (t v : 'nT[R]_([:: u.+1]))
  : t = v <-> forall i, t^^=i = v^^=i.
Proof.
split=> [->//|eq_i]; apply/ntensorP=> i.
by move: (eq_i i)=> /tensor_nil_eqP.
Qed.

Lemma otensor_eqP {d} (t v : 'oT[R]_([:: d.+1]))
  : t = v <-> forall i, t`_=i = v`_=i.
Proof.
split=> [->//|eq_i]; apply/otensorP=> i.
by move: (eq_i i)=> /tensor_nil_eqP.
Qed.

Lemma nstackE {u us ds} (f : 'I_u.+1 -> 'T[R]_(us, ds)) i : (nstack f)^^i = f i.
Proof.
rewrite /nstack/nindex unlock.
by apply/val_inj/matrixP => x y; rewrite !mxE tensormx_indexK.
Qed.

Lemma ostackE {us d ds} (f : 'I_d.+1 -> 'T[R]_(us, ds)) i : (ostack f)`_i = f i.
Proof.
rewrite /ostack/oindex unlock.
by apply/val_inj/matrixP => x y; rewrite !mxE tensormx_indexK.
Qed.

Lemma nstack_eqE {u} (f : 'I_u.+1 -> R) i : (\tensor^^i0 => f i0)^^=i = f i.
Proof. by rewrite nstackE const_tK. Qed.

Lemma ostack_eqE {d} (f : 'I_d.+1 -> R) i : (\tensor`_i0 => f i0)`_=i = f i.
Proof. by rewrite ostackE const_tK. Qed.

End TensorIndexTheory.


Section TensorTuple.

Context {R : Type} (x : nat) (us ds : pseq).

Definition ntensor_of_tuple (t : x.+1.-tuple R) : 'nT[R]_([:: x.+1]) :=
  \tensor^^i => (tnth t i).

Definition otensor_of_tuple (t : x.+1.-tuple R) : 'oT[R]_([:: x.+1]) :=
  \tensor`_i => (tnth t i).

Definition tuple_of_ntensor (t : 'nT[R]_([:: x.+1])) :=
  [tuple t^^=i | i < x.+1].

Definition tuple_of_otensor (t : 'oT[R]_([:: x.+1])) :=
  [tuple t`_=i | i < x.+1].

Lemma ntensor_of_tupleE t i : (ntensor_of_tuple t)^^=i = tnth t i.
Proof. exact: nstack_eqE. Qed.

Lemma otensor_of_tupleE t i : (otensor_of_tuple t)`_=i = tnth t i.
Proof. exact: ostack_eqE. Qed.

Lemma ntensor_of_tupleK : cancel ntensor_of_tuple tuple_of_ntensor.
Proof.
by move=> t; apply/eq_from_tnth=> i; rewrite tnth_mktuple nstack_eqE.
Qed.

Lemma tuple_of_ntensorK : cancel tuple_of_ntensor ntensor_of_tuple.
Proof.
by move=> t; apply/ntensor_eqP=> i; rewrite nstackE const_tK tnth_mktuple.
Qed.

Lemma otensor_of_tupleK : cancel otensor_of_tuple tuple_of_otensor.
Proof.
by move=> t; apply/eq_from_tnth=> i; rewrite tnth_mktuple ostack_eqE.
Qed.

Lemma tuple_of_otensorK : cancel tuple_of_otensor otensor_of_tuple.
Proof.
by move=> t; apply/otensor_eqP=> i; rewrite ostackE const_tK tnth_mktuple.
Qed.

Definition nstack_tuple (t : x.+1.-tuple 'T[R]_(us, ds)) :=
  \tensor^^i tnth t i.

Definition ostack_tuple (t : x.+1.-tuple 'T[R]_(us, ds)) :=
  \tensor`_i tnth t i.

Definition nunstack_tuple (t : 'T[R]_(x.+1 :: us, ds)) :=
  [tuple t^^i | i < x.+1].

Definition ounstack_tuple (t : 'T[R]_(us, x.+1 :: ds)) :=
  [tuple t`_i | i < x.+1].

Lemma nstack_tupleE t i : (nstack_tuple t)^^i = tnth t i.
Proof. exact: nstackE. Qed.

Lemma ostack_tupleE t i : (ostack_tuple t)`_i = tnth t i.
Proof. exact: ostackE. Qed.

Lemma nstack_tupleK : cancel nstack_tuple nunstack_tuple.
Proof. by move=> t; apply/eq_from_tnth=> i; rewrite tnth_mktuple nstackE. Qed.

Lemma nunstack_tupleK : cancel nunstack_tuple nstack_tuple.
Proof. by move=> t; apply/ntensorP=> i; rewrite nstackE tnth_mktuple. Qed.

Lemma ostack_tupleK : cancel ostack_tuple ounstack_tuple.
Proof. by move=> t; apply/eq_from_tnth=> i; rewrite tnth_mktuple ostackE. Qed.

Lemma ounstack_tupleK : cancel ounstack_tuple ostack_tuple.
Proof. by move=> t; apply/otensorP=> i; rewrite ostackE tnth_mktuple. Qed.

End TensorTuple.


Notation "[ 'tensor' ^^ t ; .. ; tn ]" :=
  (nstack_tuple [tuple of t :: .. [:: tn] ..]).
Notation "[ 'tensor' ^^= x ; .. ; xn ]" :=
  (ntensor_of_tuple [tuple of x :: .. [:: xn] ..]).
Notation "[ 'tensor' `_ t ; .. ; tn ]" :=
  (ostack_tuple [tuple of t :: .. [:: tn] ..]).
Notation "[ 'tensor' `_= x ; .. ; xn ]" :=
  (otensor_of_tuple [tuple of x :: .. [:: xn] ..]).


Section TensorMatrix.

Context {R : Type} {n m : nat}.

Definition tensor_of_matrix (M : 'M_(_, _)) : 'T[R]_([:: n.+1], [:: m.+1]) :=
  \tensor^^i \tensor`_j => M i j.

Definition matrix_of_tensor t : 'M[R]_(n.+1, m.+1) :=
  \matrix_(i, j) t^^i`_=j.

Lemma tensor_of_matrixK : cancel tensor_of_matrix matrix_of_tensor.
Proof. by move=> M; apply/matrixP=> i j; rewrite mxE nstackE ostack_eqE. Qed.

Lemma matrix_of_tensorK : cancel matrix_of_tensor tensor_of_matrix.
Proof.
move=> T; apply/ntensorP=> i; apply/otensor_eqP=> j.
by rewrite nstackE ostack_eqE mxE.
Qed.

End TensorMatrix.
