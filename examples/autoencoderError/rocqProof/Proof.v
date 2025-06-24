From mathcomp Require Import all_ssreflect all_algebra reals.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.
Require Import vehicle.tensor.
Require Import autoencoderErrorSpec.
Import Order.POrderTheory.
Import Num.Theory.
Open Scope ring_scope.
Open Scope order_scope.

Notation R := autoencoderErrorSpec.R.

Definition maxValue : tensor R [:: 5] := foreach (fun=> 1).

Definition minValue : tensor R [:: 5] := foreach (fun=> 0).

Lemma closure : forall i x, let
    y := decode (encode x) in
    tnth minValue i <= tnth x i <= tnth maxValue i
    -> tnth minValue i - tnth epsilon i <= tnth y i <= tnth maxValue i + tnth epsilon i.
Proof.
    move=> i x y /andP [mx Mx]. have [Im IM] := identity i x. apply /andP; split; rewrite /y.
    - apply /le_trans; last by apply Im.
      apply lerB. by apply mx. by [].
    - apply /le_trans; first by apply IM.
      apply lerD. by apply Mx. by [].
Qed.
