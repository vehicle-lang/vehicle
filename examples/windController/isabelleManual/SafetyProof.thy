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
  where "controllerFun p1 p2 = (lookup (Rep_OutputVector (controller (Abs_InputVector (tensor_from_vec [2] [p1, p2])))) [0])"


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
  have "forallIndex [2]
     (\<lambda>i. - 13 / 4
           \<le> lookup
               (Rep_InputVector
                 (Abs_InputVector (tensor_from_vec [2] [x, y])))
               i \<and>
           - 13 / 4
           \<le> lookup
               (Rep_InputVector
                 (Abs_InputVector (tensor_from_vec [2] [x, y])))
               i)"
    using assms
    unfolding roadWidth_def maxSensorError_def tensor_from_vec_def
    apply simp
  proof -
    have "(dims (Abs_tensor ([2], [x, y]))) = [2]"
      unfolding dims_def
      by (simp add: Abs_tensor_inverse)

    then show "\<bar>x\<bar> * 4 \<le> 13 \<Longrightarrow>
    \<bar>y\<bar> * 4 \<le> 13 \<Longrightarrow>
    forallIndex [2]
     (\<lambda>i. - (13 / 4)
           \<le> lookup
               (Rep_InputVector
                 (Abs_InputVector (Abs_tensor ([2], [x, y]))))
               i)"
      apply (simp add: Abs_InputVector_inverse)
      unfolding lookup_def dims_def vec_def
      apply (simp add: Abs_tensor_inverse)
      unfolding forallIndex_def list_all_def vec_def foreach_def tensor_from_lookup_def tensor_from_vec_def tensor_vec_from_lookup.simps fixed_length_sublist_def lookup_base.simps
      apply (simp add: Abs_tensor_inverse)
    proof (rule ballI)
      fix xa
      show " \<bar>x\<bar> * 4 \<le> 13 \<Longrightarrow>
          \<bar>y\<bar> * 4 \<le> 13 \<Longrightarrow>
          xa \<in> {0..<2} \<Longrightarrow> - (13 / 4) \<le> hd (drop xa [x, y])"
        apply (cases "xa = 0")
         apply simp
        apply (cases "xa = 1")
         apply simp
        by simp
    qed
  qed
  thus ?thesis
    unfolding controllerFun_def
    using safe
    unfolding safe_def safeInput_def safeOutput_def Let_def
    apply (erule_tac x="(Abs_InputVector (tensor_from_vec [2] [x, y]))" in allE)
  proof simp
    define out where "out = lookup
       (Rep_OutputVector
         (controller
           (Abs_InputVector (tensor_from_vec [2] [x, y])))) [0]"

    define prev where "prev = lookup
     (Rep_InputVector
       (Abs_InputVector (tensor_from_vec [2] [x, y])))
     previousSensor"

    define cur where "cur = lookup
           (Rep_InputVector
             (Abs_InputVector (tensor_from_vec [2] [x, y])))
           currentSensor"

    have "
    - (5 / 4) < out + 2 * cur - prev \<and>
    out * 4 + 8 * cur - prev * 4 < 5 \<Longrightarrow> \<bar>out + 2 * x - y\<bar> * 4
      < 5"
    proof -
      assume assm1: "- (5 / 4) < out + 2 * cur - prev \<and>
    out * 4 + 8 * cur - prev * 4 < 5"
      have fact0: "(dims (Abs_tensor ([2], [x, y]))) = [2]"
        unfolding dims_def
        by (simp add: Abs_tensor_inverse)
      have fact1: "cur = x"
        using fact0
        unfolding cur_def tensor_from_vec_def currentSensor_def
        apply (simp add: Abs_InputVector_inverse)
        unfolding lookup_def dims_def vec_def
        apply (simp add: Abs_tensor_inverse)
        unfolding fixed_length_sublist_def
        by auto
      have fact2: "prev = y"
        using fact0
        unfolding prev_def tensor_from_vec_def previousSensor_def
        apply (simp add: Abs_InputVector_inverse)
        unfolding lookup_def dims_def vec_def
        apply (simp add: Abs_tensor_inverse)
        unfolding fixed_length_sublist_def
        by auto
      show ?thesis
        using assm1
        using fact1 fact2
        by argo
    qed
      

    then show "forallIndex [2]
     (\<lambda>i. - (13 / 4)
           \<le> lookup
               (Rep_InputVector
                 (Abs_InputVector (tensor_from_vec [2] [x, y])))
               i) \<Longrightarrow>
    - (5 / 4)
    < lookup
       (Rep_OutputVector
         (controller
           (Abs_InputVector (tensor_from_vec [2] [x, y]))))
       WindControllerSpec.velocity +
      2 *
      lookup
       (Rep_InputVector
         (Abs_InputVector (tensor_from_vec [2] [x, y])))
       currentSensor -
      lookup
       (Rep_InputVector
         (Abs_InputVector (tensor_from_vec [2] [x, y])))
       previousSensor \<and>
    lookup
     (Rep_OutputVector
       (controller
         (Abs_InputVector (tensor_from_vec [2] [x, y]))))
     WindControllerSpec.velocity *
    4 +
    8 *
    lookup
     (Rep_InputVector
       (Abs_InputVector (tensor_from_vec [2] [x, y])))
     currentSensor -
    lookup
     (Rep_InputVector
       (Abs_InputVector (tensor_from_vec [2] [x, y])))
     previousSensor *
    4
    < 5 \<Longrightarrow>
    \<bar>lookup
      (Rep_OutputVector
        (controller
          (Abs_InputVector (tensor_from_vec [2] [x, y]))))
      [0] +
     2 * x -
     y\<bar> *
    4
    < 5"
      unfolding out_def WindControllerSpec.velocity_def prev_def cur_def
      by simp
      
  qed
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