theory Vehicle
  imports
    Complex_Main
    "Deep_Learning.Tensor"
    "Deep_Learning.Tensor_Subtensor"
    "Deep_Learning.Tensor_Plus"
    "Deep_Learning.Tensor_Scalar_Mult"
begin

named_theorems tensor_0dim_arithmetic "rules for simplifying arithmetic tensor operations"
named_theorems tensor_arithmetic "rules for simplifying arithmetic tensor operations"
named_theorems tensor_ops "rules for simplifying arithmetic tensor operations"

named_theorems index_ops "rules for simplifying arithmetic index operations"

typedef 'a FlexTensor = "(UNIV :: 'a tensor set)" by auto

declare [[coercion Rep_FlexTensor]]

declare Abs_FlexTensor_inverse[simp]
declare Abs_tensor_inverse[tensor_ops]

typedef FlexIndex = "(UNIV :: nat set)" by auto

declare [[coercion Rep_FlexIndex]]

declare Abs_FlexIndex_inverse[simp]

declare subtensor_def[tensor_ops]
declare fixed_length_sublist_def[tensor_ops]

definition flextensor_from_vec :: " nat list \<Rightarrow> 'a list \<Rightarrow> 'a FlexTensor"
  where[simp]: "flextensor_from_vec a b = Abs_FlexTensor ( tensor_from_vec a b)"

definition [tensor_arithmetic]: "vec_times a b = map (\<lambda>(x,y). times x y) (zip a b)"

definition tensor_plus :: "('a::semigroup_add) tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "tensor_plus A B = Abs_FlexTensor (A + B)"

definition flex_subtensor::"'a tensor \<Rightarrow> nat \<Rightarrow> 'a FlexTensor"
  where[tensor_ops]: "flex_subtensor n t = Abs_FlexTensor (subtensor n t)"

declare plus_def[tensor_arithmetic]
declare plus_base_def[tensor_arithmetic]
declare vec_plus_def[tensor_arithmetic]

definition tensor_cdot :: "('a::ring) \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "tensor_cdot a B = Abs_FlexTensor (a \<cdot> B)"

declare smult_def[tensor_arithmetic]
declare vec_smult_def[tensor_arithmetic]

definition times_base::"'a::semigroup_mult tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "times_base A B = Abs_FlexTensor (tensor_from_vec (dims A) (vec_times (vec A) (vec B)))"

definition hadamard_prod ::"'a::semigroup_mult tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "hadamard_prod A B = Abs_FlexTensor (if (dims A = dims B)
                                then times_base A B
                                else undefined)"

definition [tensor_arithmetic]:"vec_div a b = map (\<lambda>(x,y). divide x y) (zip a b)"

definition div_base::"'a::divide tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "div_base A B = Abs_FlexTensor (tensor_from_vec (dims A) (vec_div (vec A) (vec B)))"

definition pointwise_div ::"'a::divide tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "pointwise_div A B = Abs_FlexTensor (if (dims A = dims B)
                                then div_base A B
                                else undefined)"

definition [tensor_arithmetic]:"vec_min a b = map (\<lambda>(x,y). min x y) (zip a b)"

definition [tensor_arithmetic]:"vec_max a b = map (\<lambda>(x,y). max x y) (zip a b)"

definition min_base::"'a::ord tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "min_base A B = Abs_FlexTensor (tensor_from_vec (dims A) (vec_min (vec A) (vec B)))"

definition pointwise_min ::"'a::ord tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "pointwise_min A B = Abs_FlexTensor (if (dims A = dims B)
                                then min_base A B
                                else undefined)"

definition max_base::"'a::ord tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "max_base A B = Abs_FlexTensor (tensor_from_vec (dims A) (vec_max (vec A) (vec B)))"

definition pointwise_max ::"'a::ord tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[tensor_arithmetic]: "pointwise_max A B = Abs_FlexTensor (if (dims A = dims B)
                                then max_base A B
                                else undefined)"

definition combine_subtensors :: "'a tensor list \<Rightarrow> 'a FlexTensor"
  where[tensor_ops]: "combine_subtensors As = Abs_FlexTensor (subtensor_combine (dims (hd As)) As)"

definition foreach :: "nat \<Rightarrow> (nat \<Rightarrow> 'a tensor) \<Rightarrow> 'a FlexTensor"
  where[tensor_ops]: "foreach n f = Abs_FlexTensor (let tensor_results = (map f [0..<n]) in
          subtensor_combine (dims (hd tensor_results)) tensor_results)"

declare upt_def[tensor_ops]


definition forallInList :: "('a \<Rightarrow> bool) \<Rightarrow> 'a list \<Rightarrow> bool"
  where[tensor_ops]: "forallInList f l = list_all f l"

definition existsInList :: "('a \<Rightarrow> bool) \<Rightarrow> 'a list \<Rightarrow> bool"
  where[tensor_ops]: "existsInList f l = list_ex f l"

declare list_all_def[tensor_ops]
declare list_ex_iff[tensor_ops]

definition forallIndex :: "nat \<Rightarrow> (FlexIndex \<Rightarrow> bool) \<Rightarrow> bool"
  where[tensor_ops]: "forallIndex I f = (list_all id (vec (foreach I (\<lambda> x . (tensor_from_vec [] [f (Abs_FlexIndex x)])))))"

definition existsIndex :: "nat \<Rightarrow> (FlexIndex \<Rightarrow> bool) \<Rightarrow> bool"
  where[tensor_ops]: "existsIndex I f = (list_ex id (vec (foreach I (\<lambda> x . (tensor_from_vec [] [f (Abs_FlexIndex x)])))))"

(* TODO: foreachTuple ? ? ? *)
fun foreachTuple :: "nat \<Rightarrow> (nat \<Rightarrow> 'a) \<Rightarrow> 'a list" where[simp]:
    "foreachTuple 0 f = []"
  | "foreachTuple n f = (f n) # (foreachTuple (n-1) f)"


definition reduceAnd :: "bool tensor \<Rightarrow> bool \<Rightarrow> bool"
  where[tensor_ops]: "reduceAnd A i = fold (\<and>) (vec A) i"

definition reduceOr :: "bool tensor \<Rightarrow> bool \<Rightarrow> bool"
  where[tensor_ops]: "reduceOr A i = fold (\<or>) (vec A) i"

definition reduceSum :: "('a::plus) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where[tensor_ops]: "reduceSum A i = fold (+) (vec A) i"

definition reduceMul :: "('a::times) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where[tensor_ops]: "reduceMul A i = fold (*) (vec A) i"

definition reduceMin :: "('a::ord) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where[tensor_ops]: "reduceMin A i = fold (min) (vec A) i"

definition reduceMax :: "('a::ord) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where[tensor_ops]: "reduceMax A i = fold (max) (vec A) i"

definition eqTensorReduced :: "'a tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[tensor_ops]: "eqTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) = (lookup B i)))) True"

definition neTensorReduced :: "'a tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[tensor_ops]: "neTensorReduced A B = (\<not>(eqTensorReduced A B))"

definition leqTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[tensor_ops]: "leqTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) <= (lookup B i)))) True"

definition ltTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[tensor_ops]: "ltTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) < (lookup B i)))) True"

definition geqTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[tensor_ops]: "geqTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) >= (lookup B i)))) True"

definition gtTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[tensor_ops]: "gtTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) > (lookup B i)))) True"

declare dims_def[tensor_ops]
declare vec_def[tensor_ops]
declare tensor_from_vec_def[tensor_ops]

lemma tensor_plus_0dim[tensor_0dim_arithmetic]:
  "(tensor_plus (Abs_tensor ([], [x])) (Abs_tensor ([], [y]))) = Abs_FlexTensor (Abs_tensor ([], [x+y]))"
  by (simp add: tensor_arithmetic tensor_ops)

lemma tensor_hadamrad_0dim[tensor_0dim_arithmetic]:
  "(hadamard_prod (Abs_tensor ([], [x])) (Abs_tensor ([], [y]))) = Abs_FlexTensor (Abs_tensor ([], [x*y]))"
  by (simp add: tensor_arithmetic tensor_ops)

lemma tensor_tensor_cdot_0dim[tensor_0dim_arithmetic]:
  "(tensor_cdot x (Abs_tensor ([], [y]))) = Abs_FlexTensor (Abs_tensor ([], [x*y]))"
  by (simp add: tensor_arithmetic tensor_ops)

lemma tensor_div_0dim[tensor_0dim_arithmetic]:
  "(pointwise_div (Abs_tensor ([], [x])) (Abs_tensor ([], [y]))) = Abs_FlexTensor (Abs_tensor ([], [x/y]))"
  by (simp add: tensor_arithmetic tensor_ops)

end
