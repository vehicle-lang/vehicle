theory SafetyProof
  imports
    Complex_Main
    "Deep_Learning.Tensor"
    "Vehicle.Vehicle"
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

lemma InputVector_tensor_rewrite1[simp]: "(Rep_tensor (Rep_InputVector (Abs_InputVector (Abs_tensor ([2],[x1,x2]))))) =  ([2],[x1,x2])"
  by simp

lemma OutputVector_tensor_rewrite1[simp]:
  "(Rep_tensor (Rep_OutputVector (Abs_OutputVector (Abs_tensor ([1],[x1]))))) =  ([1],[x1])"
  by simp

lemma InputVector_tensor_rewrite2[simp]: "(Rep_tensor (Rep_InputVector (Abs_InputVector (Abs_tensor ([Suc (Suc 0)],[x1,x2]))))) =  ([Suc (Suc 0)],[x1,x2])"
 by simp

lemma OutputVector_tensor_rewrite2[simp]:
  "(Rep_tensor (Rep_OutputVector (Abs_OutputVector (Abs_tensor ([Suc 0],[x1]))))) =  ([Suc 0],[x1])"
  by simp


lemma tensor_plus_dim:
  assumes "dims x = dims y"
  shows "dims (tensor_plus x y) = dims x"
  unfolding tensor_plus_def
  unfolding dims_def
  unfolding plus_def
  using assms
  apply simp
  unfolding plus_base_def
  unfolding tensor_from_vec_def
  using Rep_tensor[of x] Rep_tensor[of y]
  unfolding vec_plus_def vec_def
  using Abs_tensor_inverse[of "(dims x, map2 (+) (snd (Rep_tensor x)) (snd (Rep_tensor y)))"]
  by (simp add: dims_def)


lemma controller_lem:
  assumes "(abs x) \<le> roadWidth + maxSensorError"
      and "(abs y) \<le> roadWidth + maxSensorError"
    shows "(abs ((controllerFun x y) + 2*x - y)) <
              roadWidth - maxWindShift - 3*maxSensorError"
proof -
  define X where "X = Abs_InputVector (tensor_from_vec [2] [x,y])"
  have "   forallIndex 2 (\<lambda>x. leqTensorReduced
            (Rep_FlexTensor
              (tensor_cdot (- 1)
                (Rep_FlexTensor (flextensor_from_vec [] [13 / 4]))))
            (Rep_FlexTensor
              (flex_subtensor (Rep_InputVector X) (Rep_FlexIndex x))) \<and>
           leqTensorReduced
            (Rep_FlexTensor
              (flex_subtensor (Rep_InputVector X) (Rep_FlexIndex x)))
            (Rep_FlexTensor (flextensor_from_vec [] [13 / 4])))"
    using assms
    unfolding forallIndex_def foreach_def upt_def X_def
    apply simp
    unfolding tensor_from_lookup_def tensor_vec_from_lookup.simps
    apply (simp add: tensor_ops tensor_0dim_arithmetic)
    apply (simp add: tensor_from_lookup_def tensor_vec_from_lookup.simps upt_def tensor_ops)
    apply (simp add: tensor_arithmetic tensor_ops lookup_def lookup_base.simps)
    apply (simp add: subtensor_combine_def tensor_ops lookup_base.simps)
    by linarith
  then have outputSafe: "safeOutput controller X"
    using safe
    unfolding safeInput_def
    apply (erule_tac x="X" in allE)
    by simp

  have dimFact: "order (Rep_OutputVector (controller (normalise controller (Abs_InputVector (tensor_from_vec [2] [x, y]))))) = 1"
    using Rep_OutputVector by force

  have dimFact2: "\<And> x. (fst (Rep_tensor (Rep_InputVector x))) = [2]"
    using Rep_InputVector
    unfolding dims_def
    by force

  have dimsFact3: "\<And> x . (dims (subtensor (Rep_InputVector x) currentSensor)) = []"
    unfolding dims_def subtensor_def tensor_from_vec_def
    unfolding fixed_length_sublist_def vec_def currentSensor_def prod_list_def
    apply (simp add: dimFact2)
    using Abs_tensor_inverse dimFact2
    by (smt (verit, ccfv_threshold) One_nat_def Rep_tensor Suc_1 Zero_neq_Suc length_Cons list.size(3) mem_Collect_eq mult.right_neutral prod_list.Cons prod_list.Nil prod_list_def split_pairs take0 take_Suc)

  have dimsFact4: "\<And> x . (dims (subtensor (Rep_InputVector x) previousSensor)) = []"
    unfolding dims_def subtensor_def tensor_from_vec_def
    unfolding fixed_length_sublist_def vec_def previousSensor_def prod_list_def
    apply (simp add: dimFact2)
    using Abs_tensor_inverse dimFact2
    by (smt (verit, ccfv_SIG) One_nat_def Rep_tensor Suc_1 diff_Suc_Suc diff_zero length_Cons length_drop length_take list.size(3) mem_Collect_eq numeral_1_eq_Suc_0 numeral_Bit0_eq_double prod_list.Cons prod_list.Nil prod_list_def split_pairs take_Suc_Cons take_eq_Nil)

  have dimsFact5: "\<And> x .(fst (Rep_tensor (Rep_OutputVector x))) = [1]"
    using Rep_OutputVector
    unfolding dims_def
    by blast

  obtain vel where vel_def: "
      (controller
      (normalise controller (Abs_InputVector (Abs_tensor ([2], [x, y]))))) =
      (Abs_OutputVector (Abs_tensor ([1], [vel])))"
    using Rep_OutputVector[of "(controller
      (normalise controller (Abs_InputVector (Abs_tensor ([2], [x, y])))))"]
    using Rep_tensor[of "Rep_OutputVector (controller
      (normalise controller (Abs_InputVector (Abs_tensor ([2], [x, y])))))"]
    apply simp
    by (smt (verit, best) One_nat_def Rep_OutputVector_inverse Rep_tensor_inverse dimsFact5 length_0_conv length_Suc_conv mult.right_neutral prod.collapse prod_list.Cons prod_list.Nil)

  have fact1: "(controllerFun x y + 2 * x - y) < roadWidth - maxWindShift - 3 * maxSensorError"
    using outputSafe
    unfolding safeOutput_def Let_def X_def
    unfolding controllerFun_def
    apply (simp add: Let_def tensor_ops vel_def)
    unfolding currentSensor_def previousSensor_def
    unfolding WindControllerSpec.velocity_def
    apply (simp add: dimFact dimFact2 dimsFact3 dimsFact4 dimsFact5 tensor_ops tensor_0dim_arithmetic)
    by (simp add: tensor_from_lookup_def tensor_vec_from_lookup.simps tensor_ops lookup_def lookup_base.simps)


  then have fact2: "(controllerFun x y + 2 * x - y) > -(roadWidth - maxWindShift - 3 * maxSensorError)"
    using outputSafe
    unfolding safeOutput_def Let_def X_def
    unfolding controllerFun_def
    apply (simp add: Let_def tensor_ops vel_def)
    unfolding currentSensor_def previousSensor_def
    unfolding WindControllerSpec.velocity_def
    apply (simp add: dimFact dimFact2 dimsFact3 dimsFact4 dimsFact5 tensor_ops tensor_0dim_arithmetic)
    by (simp add: tensor_from_lookup_def tensor_vec_from_lookup.simps tensor_ops lookup_def lookup_base.simps)

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
