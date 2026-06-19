From mathcomp Require Import ssreflect all_boot all_algebra order reals lra.
From mathcomp Require Import interval_inference tensor.
From vehicle Require Import utils.
Import Num.Theory GRing.Theory Order.POrderTheory.

Local Open Scope ring_scope.
Local Open Scope tensor_scope.

From windController Require Import WindControllerSpec.

Import WindControllerSpec.

Notation R := WindControllerSpec.R.

(* ----------------------------------------------*)
(* Setup *)

Definition toTensor (x : R) (y : R) : 'nT[R]_[2] :=
  ntensor_of_tuple ([tuple x ; y] : (2%:posnat)%:num%R.-tuple R).

Definition roadWidth : R := 3.
Definition maxWindShift : R := 1.
Definition maxSensorError : R := 1/4.

(* ----------------------------------------------*)
(* Model Data *)

Record state := State
  { windSpeed : R
  ; position : R
  ; velocity : R
  ; sensor : R
  }.

Record observation := Observation
  { windShift : R
  ; sensorError : R
  }.

(* ----------------------------------------------*)
(* Model Transitions *)

Definition initialState :=
  {| windSpeed := 0
  ; position := 0
  ; velocity := 0
  ; sensor := 0
  |}.

Definition controller x y :=
  (controller (normalise (toTensor x y)))^^=0.

Definition nextState o s :=
  let newWindSpeed := s.(windSpeed) + o.(windShift) in
  let newPosition := s.(position) + s.(velocity) + newWindSpeed in
  let newSensor := newPosition + o.(sensorError) in
  let newVelocity := s.(velocity) + controller newSensor s.(sensor) in
  {| windSpeed := newWindSpeed
  ; position := newPosition
  ; velocity := newVelocity
  ; sensor := newSensor
  |}.

Definition finalState xs := foldr nextState initialState xs.

(* ----------------------------------------------*)
(* Definition of Correctness *)

Definition nextPosition_windShift s : R :=
  s.(position) + s.(velocity) + s.(windSpeed).

Definition onRoad s := `| s.(position) | <= roadWidth.

Definition safeDistanceFromEdge s :=
  `| nextPosition_windShift s | < roadWidth - maxWindShift.

Definition accurateSensorReading s :=
  `| s.(position) - s.(sensor) | <= maxSensorError.

Definition sensorReadingNotOffRoad s :=
  `| s.(sensor) | <= roadWidth + maxSensorError.

Definition safeState s :=
  [&& safeDistanceFromEdge s
  , accurateSensorReading s
  & sensorReadingNotOffRoad s
  ].

Definition validObservation o :=
  (`| o.(sensorError) | <= maxSensorError)
  && (`| o.(windShift) | <= maxWindShift).

(* ----------------------------------------------*)
(* Proof of Correctness *)

Lemma initialState_onRoad : onRoad initialState.
Proof. by rewrite /onRoad normr0. Qed.

Lemma initialState_safe : safeState initialState.
Proof.
apply/and3P; split.
- rewrite /safeDistanceFromEdge/nextPosition_windShift.
  by rewrite !addr0 normr0 /roadWidth /maxWindShift; lra.
- rewrite /accurateSensorReading /nextPosition_windShift.
  by rewrite subr0 normr0 /maxSensorError; lra.
- by rewrite /sensorReadingNotOffRoad normr0 /roadWidth /maxSensorError; lra.
Qed.

Lemma five_fourths :
  (roadWidth - maxWindShift - 3 * maxSensorError = 5/4)%R.
Proof. by rewrite /roadWidth /maxWindShift /maxSensorError; lra. Qed.

Lemma thirteen_fourths : ((13/4)%R = roadWidth + maxSensorError).
Proof. by rewrite /roadWidth /maxSensorError; lra. Qed.

Lemma controller_lem : forall x y,
  `|x| <= roadWidth + maxSensorError ->
  `|y| <= roadWidth + maxSensorError ->
  `|controller x y + 2 * x - y| < roadWidth - maxWindShift - 3 * maxSensorError.
Proof.
move=> x y Hx Hy.
rewrite /controller five_fourths.
rewrite real_lter_norml//=.
have toTensor_cs : (toTensor x y)^^=currentSensor = x.
  by rewrite /toTensor ntensor_of_tupleE (tnth_nth x).
have toTensor_ps : (toTensor x y)^^=previousSensor = y.
  by rewrite /toTensor ntensor_of_tupleE (tnth_nth y).
rewrite -{2 4}toTensor_cs.
rewrite -{3 6}toTensor_ps.
rewrite -(const_tK (5 / 4)) -(const_tK (_ + _)) -tensor_nilN.
apply/andP.
rewrite !tensor_nil_ltP !const_tD !const_tN [_ (2 * _)]const_tM !tensor_nilK.
apply WindControllerSpec.safe.
rewrite /safeInput thirteen_fourths.
move=> i; rewrite -!tensor_nil_leP tensor_nilN const_tK; apply/andP.
rewrite -real_lter_norml //=.
by move: i=> [[|[|//]] ?]; rewrite /toTensor ntensor_of_tupleE.
Qed.

Lemma valid_imp_nextState_accurateSensor :
  forall o, validObservation o ->
  forall s, accurateSensorReading (nextState o s).
Proof.
move=> o /andP[valid_sensor _] s.
by rewrite /accurateSensorReading opprD addrA addrC addrN addr0 normrN.
Qed.


Lemma valid_and_safe_imp_nextState_onRoad :
  forall o, validObservation o ->
  forall s, safeState s ->
  onRoad (nextState o s).
Proof.
move=> o /andP[_ ?] s /and3P[safe_dist _ _].
rewrite /onRoad/= addrA.
apply /le_trans; first by apply ler_normD.
rewrite -lerBrDr.
apply /le_trans; first apply ltW; first by apply: safe_dist.
by apply lerB.
Qed.

Lemma valid_and_safe_imp_nextState_sensorReading_not_off_road :
  forall s, safeState s ->
  forall o, validObservation o ->
  sensorReadingNotOffRoad (nextState o s).
Proof.
move=> s safe o valid.
move: valid (valid_and_safe_imp_nextState_onRoad o valid s safe)=> /andP[? _] ?.
rewrite /sensorReadingNotOffRoad.
apply: le_trans; first by apply ler_normD.
by apply lerD.
Qed.

Lemma valid_and_safe_imp_nextState_safeDistanceFromEdge :
  forall o, validObservation o ->
  forall s, safeState s ->
  safeDistanceFromEdge (nextState o s).
Proof.
move=> o valid s safe.
move: valid safe
  (valid_and_safe_imp_nextState_sensorReading_not_off_road s safe o valid)
  => /andP[? _] /and3P[_ ? ?] next_onRoad.
rewrite /safeDistanceFromEdge /nextPosition_windShift/=.
remember (position s + velocity s + (windSpeed s + windShift o) + sensorError o)
  as x.
rewrite !addrA.
rewrite (_ :
  position s + velocity s + windSpeed s + windShift o + velocity s
    + controller x (sensor s) + windSpeed s + windShift o
  = (controller x (sensor s) + 2 * x - sensor s)
    + (sensor s - position s - sensorError o - sensorError o));
  first by lra.
apply: le_lt_trans; first by apply ler_normD.
apply: (@le_lt_trans _ _ (
  `|controller x (sensor s) + 2 * x - sensor s|
  + (maxSensorError + maxSensorError + maxSensorError)
  )).
  apply: lerD=>//.
  apply: le_trans; first by apply ler_normD.
  apply: lerD; last by rewrite normrN.
  apply: le_trans; first by apply ler_normD.
  apply: lerD; last by rewrite normrN.
  by rewrite -normrN opprD opprK addrC.
rewrite -ltrBrDr
  (_ : maxSensorError + maxSensorError + maxSensorError = 3 * maxSensorError);
    first by lra.
apply controller_lem=>//.
by rewrite Heqx /next_onRoad.
Qed.

Lemma safe_imp_nextState_safe :
  forall s, safeState s ->
  forall o, validObservation o ->
  safeState (nextState o s).
Proof.
move=> s safe o valid. apply/and3P; split.
- by apply valid_and_safe_imp_nextState_safeDistanceFromEdge.
- by apply valid_imp_nextState_accurateSensor.
- by apply valid_and_safe_imp_nextState_sensorReading_not_off_road.
Qed.

Lemma finalState_safe :
  forall xs, (forall x, List.In x xs -> validObservation x) ->
  safeState (finalState xs).
Proof.
elim=> [|x xs' IH] all_valid; first by apply initialState_safe.
apply safe_imp_nextState_safe.
  by apply IH=> ? ?; apply all_valid; right.
by apply all_valid; left.
Qed.

Theorem finalState_onRoad :
  forall xs, (forall x, List.In x xs -> validObservation x) ->
  onRoad(finalState xs).
Proof.
elim=> [|x xs' IH] all_valid; first by apply initialState_onRoad.
apply valid_and_safe_imp_nextState_onRoad.
  by apply all_valid; left.
by apply finalState_safe=> ? ?; apply all_valid; right.
Qed.
