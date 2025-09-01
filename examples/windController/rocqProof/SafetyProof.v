From mathcomp Require Import ssreflect all_boot all_algebra order reals lra.
From vehicle Require Import tensor.
Import Num.Theory GRing.Theory Order.POrderTheory.

Open Scope ring_scope.

Require Import WindControllerSpec.

Notation R := WindControllerSpec.R.

(* ----------------------------------------------*)
(* Setup *)

Definition toTensor (x : R) (y : R) := ntensor_of_tuple [tuple x ; y].

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

Lemma controller_lem : forall x y,
  `|x| <= roadWidth + maxSensorError ->
  `|y| <= roadWidth + maxSensorError ->
  `|controller x y + 2 * x - y| < roadWidth - maxWindShift - 3 * maxSensorError.
Proof.
move=> x y Hx Hy.
rewrite /controller.
rewrite (_ : roadWidth - maxWindShift - 3 * maxSensorError = (5/4))%R;
  last by rewrite /roadWidth /maxWindShift /maxSensorError; lra.
rewrite real_lter_norml//=.
rewrite {2 4}(_ : x = (toTensor x y)^^=currentSensor);
  last by rewrite ntensor_of_tupleE.
rewrite {3 6}(_ : y = (toTensor x y)^^=previousSensor);
  last by rewrite ntensor_of_tupleE.
rewrite -(const_tK (5 / 4)) -(const_tK (_ + _)) -tensor_nilN.
apply/andP.
rewrite !tensor_nil_ltP !const_tD !const_tN [_ (2 * _)]const_tM !tensor_nilK.
apply WindControllerSpec.safe.
rewrite /safeInput (_ : 13/4 = roadWidth + maxSensorError);
  last by rewrite /roadWidth /maxSensorError; lra.
move=> i; rewrite -!tensor_nil_leP tensor_nilN const_tK; apply/andP.
rewrite -real_lter_norml; last by apply num_real.
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
  last by lra.
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
    last by lra.
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
