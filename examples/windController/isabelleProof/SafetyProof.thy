theory SafetyProof
  imports
    Complex_Main
    "Deep_Learning.Tensor"
    "Vehicle"
    "WindControllerSpec"
begin

definition roadWidth :: real
  where[simp]: "roadWidth = 3"

definition maxWindShift :: real
  where[simp]: "maxWindShift=1"

definition maxSensorError :: real
  where[simp]: "maxSensorError = 1/4"

record state =
  windSpeed :: real
  position :: real
  velocity :: real
  sensor :: real

record observation =
  windShift :: real
  sensorError :: real

definition initialState :: "state"
  where "initialState = \<lparr> windSpeed = 0, position=0, velocity=0, sensor=0 \<rparr>"


definition nextPosition_windShift :: "state \<Rightarrow> real"
  where "nextPosition_windShift s = ((position s) + (velocity s) + (windSpeed s))"

definition onRoad :: "state \<Rightarrow> bool"
  where "onRoad s = ((abs (position s)) \<le> roadWidth)"

definition safeDistanceFromEdge :: "state \<Rightarrow> bool"
  where "safeDistanceFromEdge s = ((abs (nextPosition_windShift s)) < roadWidth - maxWindShift)"

definition accurateSensorReading :: "state \<Rightarrow> bool"
  where "accurateSensorReading s = ((abs ((position s) - (sensor s))) \<le> maxSensorError)"

definition sensorReadingNotOffRoad :: "state \<Rightarrow> bool"
  where "sensorReadingNotOffRoad s = ((abs (sensor s)) \<le> (roadWidth + maxSensorError))"

definition safeState :: "state \<Rightarrow> bool"
  where "safeState s = ((safeDistanceFromEdge s) \<and> (accurateSensorReading s) \<and> (sensorReadingNotOffRoad s))"

definition validObservation :: "observation \<Rightarrow> bool"
  where "validObservation obs = (
      ((abs (sensorError obs)) \<le> maxSensorError) \<and>
      ((abs (windShift obs)) \<le> maxWindShift)
    )"

context WindControllerSpec
begin

definition controllerFun :: "real \<Rightarrow> real \<Rightarrow> real"
  where "controllerFun p1 p2 = (lookup (Rep_OutputVector (controller (normalise controller (Abs_InputVector (tensor_from_vec [2] [p1, p2]))))) [WindControllerSpec.velocity])"

definition nextState :: "observation \<Rightarrow> state \<Rightarrow> state"
  where "nextState obs s = (
      let
        newWindSpeed = ((windSpeed s) + (windShift obs)) in
      let
        newPosition = ((position s) + (velocity s) + newWindSpeed) in
      let
        newSensor = (newPosition + (sensorError obs)) in
      let
        newVelocity = ((velocity s) + controllerFun newSensor (sensor s)) in
        \<lparr>
        windSpeed = newWindSpeed,
        position = newPosition,
        velocity = newVelocity,
        sensor=newSensor
        \<rparr>)"

definition finalState :: "observation list \<Rightarrow> state"
  where "finalState xs = foldr nextState xs initialState"


(* Correctness Proof *)

lemma initialState_onRoad:
  "onRoad initialState"
  by (simp add: initialState_def onRoad_def)

lemma initialState_safe:
  "safeState initialState"
  unfolding safeState_def initialState_def
  apply (rule conjI)
  unfolding safeDistanceFromEdge_def nextPosition_windShift_def
   apply simp
  apply (rule conjI)
  unfolding accurateSensorReading_def
   apply simp
  unfolding sensorReadingNotOffRoad_def
  by simp



lemma controller_lem:
  assumes "(abs x) \<le> roadWidth + maxSensorError"
      and "(abs y) \<le> roadWidth + maxSensorError"
    shows "(abs ((controllerFun x y) + 2*x - y)) <
              roadWidth - maxWindShift - 3*maxSensorError"
proof -
  define X where "X = Abs_InputVector (tensor_from_vec [2] [x,y])"
  have "forallIndex 2
       (\<lambda>i. leqTensorReduced (- 1 \<cdot> tensor_from_vec [1] [13 / 4]) (subtensor_lookup (Rep_InputVector X) i) \<and>
             leqTensorReduced (subtensor_lookup (Rep_InputVector X) i) (tensor_from_vec [1] [13 / 4]))"
    using assms
    unfolding forallIndex_def foreach_def upt_def X_def
    apply simp
    unfolding tensor_from_lookup_def tensor_vec_from_lookup.simps
    apply (simp add: lookup_def lookup_base.simps fixed_length_sublist_def Abs_InputVector_inverse)
    by linarith
  then have outputSafe: "safeOutput controller X"
    using safe
    unfolding safeInput_def
    apply (erule_tac x="X" in allE)
    by simp

  have dimFact: "order (Rep_OutputVector (controller (normalise controller (Abs_InputVector (tensor_from_vec [2] [x, y]))))) = 1"
    using Rep_OutputVector by force

  have fact1: "(controllerFun x y + 2 * x - y) < roadWidth - maxWindShift - 3 * maxSensorError"  
    using outputSafe
    unfolding safeOutput_def Let_def
    unfolding controllerFun_def X_def
    unfolding roadWidth_def maxWindShift_def maxSensorError_def
    unfolding subtensor_lookup_def
    using dimFact
    unfolding WindControllerSpec.velocity_def currentSensor_def previousSensor_def
    apply (simp add: lookup_def lookup_base.simps fixed_length_sublist_def Abs_InputVector_inverse)
    by (simp add: tensor_from_lookup_def tensor_vec_from_lookup.simps fixed_length_sublist_def vec_plus_def)

  then have fact2: "(controllerFun x y + 2 * x - y) > -(roadWidth - maxWindShift - 3 * maxSensorError)"
    using outputSafe
    unfolding safeOutput_def Let_def
    unfolding controllerFun_def X_def
    unfolding roadWidth_def maxWindShift_def maxSensorError_def
    unfolding subtensor_lookup_def
    using dimFact
    unfolding WindControllerSpec.velocity_def currentSensor_def previousSensor_def
    apply (simp add: lookup_def lookup_base.simps fixed_length_sublist_def Abs_InputVector_inverse)
    by (simp add: tensor_from_lookup_def tensor_vec_from_lookup.simps fixed_length_sublist_def vec_plus_def)

  then show ?thesis
    using fact1 fact2
    by argo
    
qed


lemma valid_imp_nextState_accurateSensor:
  assumes "validObservation obs"
  shows "accurateSensorReading (nextState obs s)"
  using assms
  unfolding validObservation_def
  unfolding accurateSensorReading_def
  unfolding nextState_def Let_def
  by simp

lemma valid_and_safe_imp_nextState_onRoad:
  assumes "validObservation obs"
      and "safeState s"
    shows "onRoad (nextState obs s)"
  using assms
  unfolding validObservation_def safeState_def
  unfolding onRoad_def
  by (smt (verit) nextPosition_windShift_def nextState_def safeDistanceFromEdge_def state.ext_inject state.surjective)

lemma valid_and_safe_imp_nextState_sensorReading_not_off_road:
  assumes "validObservation obs"
      and "safeState s"
    shows "sensorReadingNotOffRoad (nextState obs s)"
  using assms
  unfolding validObservation_def safeState_def
  unfolding sensorReadingNotOffRoad_def
  by (smt (verit) accurateSensorReading_def assms(1) assms(2) onRoad_def valid_and_safe_imp_nextState_onRoad valid_imp_nextState_accurateSensor)

lemma valid_and_safe_imp_nextState_safeDistanceFromEdge:
  assumes "validObservation obs"
      and "safeState s"
    shows "safeDistanceFromEdge (nextState obs s)"
  using assms
  unfolding validObservation_def safeState_def
  unfolding safeDistanceFromEdge_def
  unfolding nextPosition_windShift_def
  unfolding nextState_def Let_def
  apply simp
  using controller_lem
  by (smt (z3) accurateSensorReading_def assms(1) maxWindShift_def roadWidth_def sensorReadingNotOffRoad_def validObservation_def)

lemma safe_imp_nextState_safe:
  assumes "safeState s"
      and "validObservation obs"
    shows "safeState (nextState obs s)"
  using assms
  unfolding safeState_def
  apply (intro conjI)
  using valid_and_safe_imp_nextState_safeDistanceFromEdge[of obs s]
  unfolding safeState_def
    apply simp
  using valid_imp_nextState_accurateSensor[of obs s]
   apply simp
  using valid_and_safe_imp_nextState_sensorReading_not_off_road[of obs s]
  unfolding safeState_def
  by simp

lemma finalState_safe:
  assumes "list_all validObservation xs"
  shows "safeState (finalState xs)"
  using assms safe_imp_nextState_safe initialState_safe
  unfolding finalState_def
proof (induct xs)
  case Nil
  then show ?case
    by simp
next
  case (Cons a xs)
  then show ?case
    by simp
qed

theorem finalState_onRoad:
  assumes "list_all validObservation xs"
  shows "onRoad (finalState xs)"
  using finalState_safe[of xs] assms
  unfolding finalState_def
proof (induct xs)
  case Nil
  then show ?case
    using initialState_onRoad
    by simp
next
  case (Cons a xs)
  then show ?case
    using valid_and_safe_imp_nextState_onRoad[of a]
    using finalState_def finalState_safe by auto
qed

end

end