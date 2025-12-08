theory Test
  imports
    Complex_Main
    "Deep_Learning.Tensor"
    "Vehicle.Vehicle"
    "WindControllerSpec"
begin

lemma "lookup (tensor_from_vec [2] [(1::real), 2]) [0] = 1"
  unfolding tensor_from_vec_def lookup_def dims_def vec_def
  apply (simp add: Abs_tensor_inverse)
  unfolding hd_def fixed_length_sublist_def take_def drop_def
  by auto

lemma "lookup (tensor_from_vec [2] [(1::real), 2]) [1] = 2"
  unfolding tensor_from_vec_def lookup_def dims_def vec_def
  apply (simp add: Abs_tensor_inverse)
  unfolding hd_def fixed_length_sublist_def take_def drop_def
  by auto

fun testInstantiation :: "InputVector \<Rightarrow> OutputVector"
  where "testInstantiation x = (Abs_OutputVector
      (tensor_from_vec [1] [
        (-2 * (8*(lookup (Rep_InputVector x) [currentSensor])-4) +
             (8*(lookup (Rep_InputVector x) [previousSensor])-4))

      ]))"

lemma cdot_0dim[simp]: "(dims (tensor_cdot x (flextensor_from_vec [] [y]))) = []"
  by (simp add: tensor_ops tensor_arithmetic Abs_tensor_inverse Rep_tensor_inverse)

lemma plus_0dim1[simp]:
  assumes "(dims x) = []"
  shows "(dims (tensor_plus x (flextensor_from_vec [] [y]))) = []"
  unfolding dims_def tensor_cdot_def smult_def tensor_from_vec_def
  unfolding vec_def vec_smult_def
  unfolding flextensor_from_vec_def tensor_from_vec_def
  apply (simp add: Abs_tensor_inverse Rep_tensor_inverse)
  using assms
  unfolding plus_def dims_def
  apply (simp add: tensor_ops tensor_arithmetic assms Abs_tensor_inverse)
  by (metis One_nat_def dims_def dims_tensor length_Cons length_map length_vec length_zip list.size(3) min.idem prod_list.Nil prod_list.Nil tensor_from_vec_def vec_def)

lemma plus_0dim2[simp]:
  assumes "(dims x) = (dims y)"
  shows "(dims (tensor_plus x y)) = (dims x)"
  unfolding dims_def tensor_cdot_def smult_def tensor_from_vec_def
  unfolding vec_def vec_smult_def
  unfolding flextensor_from_vec_def tensor_from_vec_def
  apply (simp add: tensor_ops tensor_arithmetic  Abs_tensor_inverse Rep_tensor_inverse)
  using assms
  unfolding plus_def dims_def
  apply (simp add: Abs_tensor_inverse)
  unfolding plus_base_def tensor_from_vec_def
  unfolding vec_plus_def vec_def
  using Abs_tensor_inverse Rep_tensor[of x] Rep_tensor[of y]
  by (metis (full_types) dims_def plus_base_def plus_def plus_dim1 tensor_from_vec_def vec_def vec_plus_def)

lemma plus_0dim3[simp]:
  assumes "(dims x) = (dims y)"
  shows "(dims (tensor_plus x y)) = (dims y)"
  using plus_0dim2
  by (simp add: assms)

lemma take_len[simp]:
  assumes "(length x > 0)"
  shows "length (take (Suc 0) x) = 1"
  by (metis One_nat_def Suc_leI Suc_le_D assms length_take min_0R min_Suc_Suc)

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


interpretation windInstance:
  WindControllerSpec testInstantiation
proof -

  have dimFact1: "\<And> x . order (Rep_OutputVector (testInstantiation (normalise testInstantiation x))) = 1"
    using Rep_OutputVector by force

  have dimFact2: "\<And> x . order (Rep_InputVector x) = 1"
    using Rep_InputVector by force

  show "WindControllerSpec testInstantiation"
    apply standard
    unfolding safeOutput_def Let_def
    unfolding WindControllerSpec.velocity_def currentSensor_def previousSensor_def
    apply (intro allI, intro impI)
  proof (intro conjI)
    fix xa
    assume inputSafe: "safeInput testInstantiation xa"

    have dimFact: "(dims (Rep_InputVector xa)) = [2]"
      using Rep_InputVector
      by simp

    have dimFact2: "[] =
     dims
      (Abs_tensor
        ([], take (Suc 0) (snd (Rep_tensor (Rep_InputVector xa)))))"
      using dimFact
      unfolding dims_def
      using Rep_InputVector Rep_tensor
      apply simp
      by (smt (verit, ccfv_SIG) Abs_tensor_inverse One_nat_def Rep_tensor Suc_1 Zero_neq_Suc fst_conv length_Cons list.size(3) mem_Collect_eq numeral_1_eq_Suc_0 numeral_Bit0_eq_double prod_list.Cons prod_list.Nil snd_conv take0 take_Suc)
    then have dimFact3: " ([] =
     dims
      (Abs_tensor
        ([],
         take (Suc 0)
          (drop (Suc 0 * 0) (snd (Rep_tensor (Rep_InputVector xa)))))))"
      by simp

    obtain cur prev :: real where xa_rewrite: "xa = (Abs_InputVector (Abs_tensor ([2],[cur, prev])))"
      using Rep_InputVector[of xa] Rep_tensor[of xa]
      apply simp
      by (smt (verit, ccfv_SIG) One_nat_def Rep_InputVector_inverse Rep_tensor_inverse Suc_1 dims_def length_0_conv length_Suc_conv numeral_1_eq_Suc_0 numeral_Bit0_eq_double prod.collapse prod_list.Cons prod_list.Nil)

    have safeInputAssm:"- (13 / 4) \<le> cur \<and> cur * 4 \<le> 13 \<and> - (13 / 4) \<le> prev \<and> prev * 4 \<le> 13"
        using inputSafe
        unfolding safeInput_def ltTensorReduced_def
        apply (simp add: xa_rewrite)
        apply (simp add: tensor_ops tensor_0dim_arithmetic)
        by (simp add: tensor_ops tensor_from_lookup_def lookup_def subtensor_combine_def lookup_base.simps)

    show "ltTensorReduced
          (Rep_FlexTensor
            (tensor_cdot (- 1)
              (Rep_FlexTensor (flextensor_from_vec [] [5 / 4]))))
          (Rep_FlexTensor
            (tensor_plus
              (Rep_FlexTensor
                (tensor_plus
                  (subtensor
                    (Rep_OutputVector
                      (testInstantiation (normalise testInstantiation xa)))
                    0)
                  (Rep_FlexTensor
                    (hadamard_prod
                      (Rep_FlexTensor (flextensor_from_vec [] [2]))
                      (subtensor (Rep_InputVector xa) 0)))))
              (Rep_FlexTensor
                (tensor_cdot (- 1) (subtensor (Rep_InputVector xa) 1)))))"
      unfolding ltTensorReduced_def reduceAnd_def
      apply (simp add: xa_rewrite)
      apply (simp add: tensor_ops)
      unfolding normalise_def testInstantiation.simps
      apply (simp add: tensor_ops)
      apply (simp add: tensor_0dim_arithmetic)
      apply (simp add: tensor_ops lookup_def subtensor_combine_def lookup_base.simps)
      unfolding InputVector_tensor_rewrite2 OutputVector_tensor_rewrite2
      unfolding currentSensor_def previousSensor_def
      apply (simp add: lookup_base.simps tensor_ops tensor_from_lookup_def)
      using safeInputAssm
      by argo

    show "ltTensorReduced
          (Rep_FlexTensor
            (tensor_plus
              (Rep_FlexTensor
                (tensor_plus
                  (subtensor
                    (Rep_OutputVector
                      (testInstantiation (normalise testInstantiation xa)))
                    0)
                  (Rep_FlexTensor
                    (hadamard_prod
                      (Rep_FlexTensor (flextensor_from_vec [] [2]))
                      (subtensor (Rep_InputVector xa) 0)))))
              (Rep_FlexTensor
                (tensor_cdot (- 1) (subtensor (Rep_InputVector xa) 1)))))
          (Rep_FlexTensor (flextensor_from_vec [] [5 / 4]))"
      unfolding ltTensorReduced_def reduceAnd_def
      apply (simp add: xa_rewrite)
      apply (simp add: tensor_ops)
      unfolding normalise_def testInstantiation.simps
      apply (simp add: tensor_ops)
      apply (simp add: tensor_0dim_arithmetic)
      apply (simp add: tensor_ops lookup_def subtensor_combine_def lookup_base.simps)
      unfolding InputVector_tensor_rewrite2 OutputVector_tensor_rewrite2
      unfolding currentSensor_def previousSensor_def
      apply (simp add: lookup_base.simps tensor_ops tensor_from_lookup_def)
      using safeInputAssm
      by argo
  qed
qed

end
