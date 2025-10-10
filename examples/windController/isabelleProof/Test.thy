theory Test
  imports
    Complex_Main
    "Deep_Learning.Tensor"
    "Vehicle"
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
    unfolding subtensor_lookup_def
    unfolding WindControllerSpec.velocity_def currentSensor_def previousSensor_def
    apply (intro allI, intro impI)
  proof -
    fix xa
    assume inputSafe: "safeInput testInstantiation xa"
    show "ltTensorReduced (- 1 \<cdot> tensor_from_vec [1] [5 / 4])
            (Rep_FlexTensor
              (tensor_plus
                (Rep_FlexTensor
                  (tensor_plus
                    (Rep_FlexTensor
                      (Abs_FlexTensor
                        (if order
                              (Rep_OutputVector
                                (testInstantiation
                                  (normalise testInstantiation xa))) =
                            1
                          then tensor_from_vec [1]
                                [lookup
                                  (Rep_OutputVector
                                    (testInstantiation
                                      (normalise testInstantiation xa)))
                                  [0]]
                          else subtensor
                                (Rep_OutputVector
                                  (testInstantiation
                                    (normalise testInstantiation xa)))
                                0)))
                    (Rep_FlexTensor
                      (hadamard_prod (tensor_from_vec [1] [2])
                        (Rep_FlexTensor
                          (Abs_FlexTensor
                            (if order (Rep_InputVector xa) = 1
                              then tensor_from_vec [1]
                                    [lookup (Rep_InputVector xa) [0]]
                              else subtensor (Rep_InputVector xa) 0)))))))
                (- 1 \<cdot>
                  Rep_FlexTensor
                  (Abs_FlexTensor
                    (if order (Rep_InputVector xa) = 1
                      then tensor_from_vec [1] [lookup (Rep_InputVector xa) [1]]
                      else subtensor (Rep_InputVector xa) 1))))) \<and>
            ltTensorReduced
            (Rep_FlexTensor
              (tensor_plus
                (Rep_FlexTensor
                  (tensor_plus
                    (Rep_FlexTensor
                      (Abs_FlexTensor
                        (if order
                              (Rep_OutputVector
                                (testInstantiation
                                  (normalise testInstantiation xa))) =
                            1
                          then tensor_from_vec [1]
                                [lookup
                                  (Rep_OutputVector
                                    (testInstantiation
                                      (normalise testInstantiation xa)))
                                  [0]]
                          else subtensor
                                (Rep_OutputVector
                                  (testInstantiation
                                    (normalise testInstantiation xa)))
                                0)))
                    (Rep_FlexTensor
                      (hadamard_prod (tensor_from_vec [1] [2])
                        (Rep_FlexTensor
                          (Abs_FlexTensor
                            (if order (Rep_InputVector xa) = 1
                              then tensor_from_vec [1]
                                    [lookup (Rep_InputVector xa) [0]]
                              else subtensor (Rep_InputVector xa) 0)))))))
                (- 1 \<cdot>
                  Rep_FlexTensor
                  (Abs_FlexTensor
                    (if order (Rep_InputVector xa) = 1
                      then tensor_from_vec [1] [lookup (Rep_InputVector xa) [1]]
                      else subtensor (Rep_InputVector xa) 1)))))
            (tensor_from_vec [1] [5 / 4])"
      using dimFact1[of xa]
      using dimFact2[of xa]
      apply simp
      unfolding testInstantiation.simps currentSensor_def previousSensor_def normalise_def
      apply simp
      apply (simp add: Rep_OutputVector_inverse Abs_OutputVector_inverse Rep_tensor_inverse Abs_tensor_inverse)
      apply (simp add: lookup_def lookup_base.simps fixed_length_sublist_def Abs_InputVector_inverse upt_def)
      apply (simp add: tensor_from_lookup_def tensor_vec_from_lookup.simps fixed_length_sublist_def vec_plus_def)
      apply (simp add: lookup_base.simps dims_def vec_def)
      apply (simp add: lookup_def lookup_base.simps fixed_length_sublist_def Abs_InputVector_inverse)
      apply (simp add: tensor_from_vec_def fixed_length_sublist_def Rep_tensor_inverse Abs_tensor_inverse)
    proof -
      define P where "P = lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
         (snd (Rep_tensor (Rep_InputVector xa))) [Suc 0]"
      define Q where "Q = lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
       (snd (Rep_tensor (Rep_InputVector xa))) [0]"

      have "- (13 / 4)
        \<le> Q \<and>
        Q *
        4
        \<le> 13 \<and>
        - (13 / 4)
        \<le> P \<and>
        P *
        4
        \<le> 13"
        using inputSafe
        unfolding safeInput_def
        using dimFact1[of xa]
      using dimFact2[of xa]
      apply simp
      unfolding testInstantiation.simps currentSensor_def previousSensor_def normalise_def
      apply simp
      apply (simp add: Rep_OutputVector_inverse Abs_OutputVector_inverse Rep_tensor_inverse Abs_tensor_inverse)
      apply (simp add: lookup_def lookup_base.simps fixed_length_sublist_def Abs_InputVector_inverse upt_def)
      apply (simp add: tensor_from_lookup_def tensor_vec_from_lookup.simps fixed_length_sublist_def vec_plus_def)
      apply (simp add: lookup_base.simps dims_def vec_def)
      unfolding P_def Q_def
      by simp

      then have "- (5 / 4)
    < 2 *
      
       Q +
      (4 +
       ((32 +
         8 *
         
          P) /
        8 -
        (64 +
         16 *
         
          Q) /
        8)) -
      
       P \<and>
    11 +
    (8 *
     
      Q +
     ((128 +
       32 *
       
        P) /
      8 +
      (- ((256 +
           64 *
           
            Q) /
          8) -
       
        P *
       4)))
    < 0"
        apply auto
         apply argo
        by argo
      then show "length (fst (Rep_tensor (Rep_InputVector xa))) = Suc 0 \<Longrightarrow>
    - (5 / 4)
    < 2 *
      lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
       (snd (Rep_tensor (Rep_InputVector xa))) [0] +
      (4 +
       ((32 +
         8 *
         lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
          (snd (Rep_tensor (Rep_InputVector xa))) [Suc 0]) /
        8 -
        (64 +
         16 *
         lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
          (snd (Rep_tensor (Rep_InputVector xa))) [0]) /
        8)) -
      lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
       (snd (Rep_tensor (Rep_InputVector xa))) [Suc 0] \<and>
    11 +
    (8 *
     lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
      (snd (Rep_tensor (Rep_InputVector xa))) [0] +
     ((128 +
       32 *
       lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
        (snd (Rep_tensor (Rep_InputVector xa))) [Suc 0]) /
      8 +
      (- ((256 +
           64 *
           lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
            (snd (Rep_tensor (Rep_InputVector xa))) [0]) /
          8) -
       lookup_base (fst (Rep_tensor (Rep_InputVector xa)))
        (snd (Rep_tensor (Rep_InputVector xa))) [Suc 0] *
       4)))
    < 0"
        unfolding Q_def P_def
        by simp
    qed
  qed
qed

end