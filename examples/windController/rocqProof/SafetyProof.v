From mathcomp Require Import all_ssreflect all_algebra reals lra.
From mathcomp.algebra_tactics Require Import ring.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.
Import Num.Theory GRing.
Require Import vehicle.tensor.

Open Scope ring_scope.

Require WindControllerSpec.

Notation R := WindControllerSpec.R.

(* ----------------------------------------------*)
(* Setup *)

Definition toTensor (x : R) (y : R) : tensor R [:: 2] := [tuple x ; y ].

Definition roadWidth : R := 3.
Definition maxWindShift : R := 1.
Definition maxSensorError : R := 1/4.

Lemma roadWidth_ge_0 :
    roadWidth >= 0.
Proof. by []. Qed.

Lemma maxWithShift_ge_0 :
    maxWindShift >= 0.
Proof. by []. Qed.

Lemma maxSensorError_ge_0 :
    maxSensorError >= 0.
Proof. by rewrite mulr_ge0// invr_ge0. Qed.

(* ----------------------------------------------*)
(* Model Data *)

Record State :=
    { windSpeed : R
    ; position : R
    ; velocity : R
    ; sensor : R
    }.

Record Observation :=
    { windShift : R
    ; sensorError : R
    }.

(* ----------------------------------------------*)
(* Model Transitions *)

Definition initialState : State := 
    {| windSpeed := 0
    ; position := 0
    ; velocity := 0
    ; sensor := 0
    |}.

Definition controller (x : R) (y : R) : R :=
    tnth (WindControllerSpec.controller (WindControllerSpec.normalise (toTensor x y))) 0.

Definition nextState (o : Observation) (s : State) : State := 
    let newWindSpeed := s.(windSpeed) + o.(windShift) in
    let newPosition := s.(position) + s.(velocity) + newWindSpeed in
    let newSensor := newPosition + o.(sensorError) in
    let newVelocity := s.(velocity) + controller newSensor s.(sensor) in
    {| windSpeed := newWindSpeed
    ; position := newPosition
    ; velocity := newVelocity
    ; sensor := newSensor
    |}.

Definition finalState (xs : seq Observation) : State := 
    foldr nextState initialState xs.

(* ----------------------------------------------*)
(* Definition of Correctness *)

Definition nextPosition_windShift (s : State) : R :=
    s.(position) + s.(velocity) + s.(windSpeed).

Definition onRoad (s : State) : Prop :=
    `| s.(position) | <= roadWidth.

Definition safeDistanceFromEdge (s : State) : Prop :=
    `| nextPosition_windShift s | < roadWidth - maxWindShift.

Definition accurateSensorReading (s : State) : Prop :=
    `| s.(position) - s.(sensor) | <= maxSensorError.

Definition sensorReadingNotOffRoad (s : State) : Prop :=
    `| s.(sensor) | <= roadWidth + maxSensorError.

Definition safeState (s : State) : Prop :=
    safeDistanceFromEdge s 
    /\ accurateSensorReading s
    /\ sensorReadingNotOffRoad s.

Definition validObservation (o : Observation) : Prop :=
    `| o.(sensorError) | <= maxSensorError
    /\ `| o.(windShift) | <= maxWindShift.

(* ----------------------------------------------*)
(* Proof of Correctness *)

Theorem initialState_onRoad : onRoad initialState.
Proof. by rewrite /onRoad normr0. Qed.

Theorem initialState_safe : safeState initialState.
Proof.
    repeat apply conj. 
    rewrite /safeDistanceFromEdge /nextPosition_windShift/= !addr0 normr0 /roadWidth /maxWindShift. by lra.
    rewrite /accurateSensorReading /nextPosition_windShift/= subr0 normr0 /maxSensorError. by lra.
    rewrite /sensorReadingNotOffRoad normr0 /roadWidth /maxSensorError. by lra.
Qed.

Lemma controller_lem :
    forall x y, 
        `| x | <= roadWidth + maxSensorError ->
        `| y | <= roadWidth + maxSensorError ->
        `| controller x y + 2 * x - y | < roadWidth - maxWindShift - 3 * maxSensorError.
Proof.
    move=> x y Hx Hy.
    rewrite /controller.
    replace (roadWidth - maxWindShift - 3 * maxSensorError) with (125 / 100 : R); 
        last by rewrite /roadWidth /maxWindShift /maxSensorError; lra.
    rewrite real_lter_norml//=.
    replace (2 * x) with (2 * tnth (toTensor x y) WindControllerSpec.currentSensor); 
        last by rewrite /WindControllerSpec.currentSensor.
    replace (- y) with (- tnth (toTensor x y) WindControllerSpec.previousSensor);
        last by rewrite /WindControllerSpec.previousSensor.
    replace 0 with WindControllerSpec.velocity; last by [].
    apply (WindControllerSpec.safe (toTensor x y)).
    rewrite /toTensor /WindControllerSpec.safeInput. 
    replace (325 / 100) with (roadWidth + maxSensorError); last by rewrite /roadWidth /maxSensorError; lra.
    elim; case. by move: Hx; rewrite real_lter_norml; last by apply num_real.
    case. move=> i. by move: Hy; rewrite real_lter_norml; last by apply num_real.
    by auto.
Qed.

Lemma valid_imp_nextState_accurateSensor : 
    forall o, validObservation o ->
    forall s, accurateSensorReading (nextState o s).
Proof.
    move=> o [H1 H2] s. rewrite /accurateSensorReading/=.
    set (v := position s + velocity s + (windSpeed s + windShift o)).
    rewrite opprD addrA addrC addrN addr0 normrN. by apply: H1.
Qed.


Lemma valid_and_safe_imp_nextState_onRoad :
    forall o, validObservation o ->
    forall s, safeState s ->
    onRoad (nextState o s).
Proof.
    rewrite /validObservation /safeState. move=> o [Hsensor Hws] s [Hsafedist [Haccsensor Hsenonroad]].
    rewrite /onRoad/= addrA.
    apply /Order.le_trans; first by apply ler_normD.
    rewrite -lerBrDr.
    rewrite /safeDistanceFromEdge in Hsafedist. 
    apply /Order.le_trans; first apply Order.POrderTheory.ltW; first by apply: Hsafedist.
    by apply lerB.
Qed.

Lemma valid_and_safe_imp_nextState_sensorReading_not_off_road :
    forall s, safeState s ->
    forall o, validObservation o ->
    sensorReadingNotOffRoad (nextState o s).
Proof.
    move=> s Hs o Ho.
    pose HnextOnRoad := valid_and_safe_imp_nextState_onRoad Ho Hs.
    move: Ho HnextOnRoad. rewrite /onRoad/=. move=> [Hsensor HwindShift] HnextOnRoad.
    rewrite /sensorReadingNotOffRoad/=.
    apply /Order.le_trans; first by apply ler_normD.
    by apply lerD.
Qed.

Lemma valid_and_safe_imp_nextState_safeDistanceFromEdge :
    forall o, validObservation o ->
    forall s, safeState s ->
    safeDistanceFromEdge (nextState o s).
Proof.
    move=> o Ho s Hs.
    pose HnextSensorOnRoad := valid_and_safe_imp_nextState_sensorReading_not_off_road Hs Ho.
    move: Ho HnextSensorOnRoad; rewrite /validObservation/sensorReadingNotOffRoad/=. move=> [Hsensor HwindShift] HnextSensorOnRoad.
    rewrite /safeDistanceFromEdge /nextPosition_windShift/=.
    remember (position s + velocity s + (windSpeed s + windShift o) + sensorError o) as x.
    repeat rewrite addrA.
    replace (position s + velocity s + windSpeed s + windShift o + velocity s +
    controller x (sensor s) + windSpeed s + windShift o) with (
        (controller x (sensor s) + 2 * x - sensor s) + (sensor s - position s - sensorError o - sensorError o)
    ); last by lra.
    apply /Order.POrderTheory.le_lt_trans; first by apply ler_normD.
    apply (@Order.POrderTheory.le_lt_trans _ _ (`|controller x (sensor s) + 2 * x - sensor s| + 3 * maxSensorError)).
    apply lerD; first by [].
    apply /Order.POrderTheory.le_trans; first apply ler_normD.
    replace (3 * maxSensorError) with (2 * maxSensorError + maxSensorError); last by lra.
    apply lerD; last by rewrite normrN.
    apply /Order.POrderTheory.le_trans; first apply ler_normD.
    replace (2 * maxSensorError) with (maxSensorError + maxSensorError); last by lra.
    apply lerD; last by rewrite normrN.
    rewrite -normrN opprD/= opprK addrC. move: Hs => [_ [HaccSensor _]]. move: HaccSensor; apply.
    rewrite -ltrBrDr. apply controller_lem; first by apply HnextSensorOnRoad.
    move: Hs => [_ [_ HsensorOnRoad]]; move: HsensorOnRoad. by apply.
Qed.

Lemma safe_imp_nextState_safe :
    forall s, safeState s ->
    forall o, validObservation o ->
    safeState (nextState o s).
Proof.
    move=> s Hs o Ho. rewrite /safeState.
    split. by apply valid_and_safe_imp_nextState_safeDistanceFromEdge.
    split. by apply valid_imp_nextState_accurateSensor.
    by apply valid_and_safe_imp_nextState_sensorReading_not_off_road.
Qed.

Lemma finalState_safe :
    forall xs, (forall x, Coq.Lists.List.In x xs -> validObservation x) ->
    safeState (finalState xs).
Proof.
    move=> xs H. induction xs.
    by apply initialState_safe.
    apply safe_imp_nextState_safe. apply IHxs. move=> x HI. apply H.
    right. by apply HI. apply (H a). by left.
Qed.

Lemma finalState_onRoad :
    forall xs, (forall x, Coq.Lists.List.In x xs -> validObservation x) ->
    onRoad(finalState xs).
Proof.
    move=> xs H. induction xs.
    by apply initialState_onRoad.
    apply valid_and_safe_imp_nextState_onRoad. apply (H a). by left.
    apply finalState_safe. move=> x HI. apply H. right. by apply HI.
Qed.
