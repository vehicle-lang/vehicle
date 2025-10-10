theory Vehicle
  imports
    Complex_Main
    "Deep_Learning.Tensor"
    "Deep_Learning.Tensor_Subtensor"
    "Deep_Learning.Tensor_Plus"
    "Deep_Learning.Tensor_Scalar_Mult"
begin

typedef 'a FlexTensor = "(UNIV :: 'a tensor set)" by auto

declare [[coercion Rep_FlexTensor]]

declare Abs_FlexTensor_inverse[simp]

definition flextensor_from_vec :: " nat list \<Rightarrow> 'a list \<Rightarrow> 'a FlexTensor"
  where "flextensor_from_vec a b = Abs_FlexTensor ( tensor_from_vec a b)"

definition [simp]: "vec_times a b = map (\<lambda>(x,y). times x y) (zip a b)"

definition tensor_plus :: "('a::semigroup_add) tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "tensor_plus A B = Abs_FlexTensor (A + B)"

definition tensor_cdot :: "('a::ring) \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "tensor_cdot a B = Abs_FlexTensor (a \<cdot> B)"

definition times_base::"'a::semigroup_mult tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "times_base A B = Abs_FlexTensor (tensor_from_vec (dims A) (vec_times (vec A) (vec B)))"

definition hadamard_prod ::"'a::semigroup_mult tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "hadamard_prod A B = Abs_FlexTensor (if (dims A = dims B)
                                then times_base A B
                                else undefined)"

definition [simp]:"vec_div a b = map (\<lambda>(x,y). divide x y) (zip a b)"

definition div_base::"'a::divide tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "div_base A B = Abs_FlexTensor (tensor_from_vec (dims A) (vec_div (vec A) (vec B)))"

definition pointwise_div ::"'a::divide tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "pointwise_div A B = Abs_FlexTensor (if (dims A = dims B)
                                then div_base A B
                                else undefined)"

definition [simp]:"vec_min a b = map (\<lambda>(x,y). min x y) (zip a b)"

definition [simp]:"vec_max a b = map (\<lambda>(x,y). max x y) (zip a b)"

definition min_base::"'a::ord tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "min_base A B = Abs_FlexTensor (tensor_from_vec (dims A) (vec_min (vec A) (vec B)))"

definition pointwise_min ::"'a::ord tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "pointwise_min A B = Abs_FlexTensor (if (dims A = dims B)
                                then min_base A B
                                else undefined)"

definition max_base::"'a::ord tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "max_base A B = Abs_FlexTensor (tensor_from_vec (dims A) (vec_max (vec A) (vec B)))"

definition pointwise_max ::"'a::ord tensor \<Rightarrow> 'a tensor \<Rightarrow> 'a FlexTensor"
  where[simp]: "pointwise_max A B = Abs_FlexTensor (if (dims A = dims B)
                                then max_base A B
                                else undefined)"

definition subtensor_lookup :: "'a tensor \<Rightarrow> nat \<Rightarrow> 'a FlexTensor"
  where[simp]: "subtensor_lookup A i = Abs_FlexTensor (
          if (length (dims A)) = 1 then
            tensor_from_vec [1] [lookup A [i]]
          else
            subtensor A i)"


(* If: Ensure all tensors have the same remaining dimensions *)
definition subtensor_concat :: "'a tensor list \<Rightarrow> 'a FlexTensor"
  where[simp]: "subtensor_concat As = Abs_FlexTensor (
    if (list_all (\<lambda> x . (tl (dims x)) = (tl (dims (hd As)))) As) then
      (tensor_from_vec
        (((fold (\<lambda> x y . (hd (dims x)) + y) As) 0) # (tl (dims (hd As))))
        (concat (map vec As))
      )
    else
      undefined
    )"

definition foreach :: "nat \<Rightarrow> (nat \<Rightarrow> 'a tensor) \<Rightarrow> 'a FlexTensor"
  where[simp]: "foreach n f = Abs_FlexTensor (let tensor_results = (map f [0..<n]) in
          subtensor_concat tensor_results)"


definition forallInList :: "'a list \<Rightarrow> ('a \<Rightarrow> bool) \<Rightarrow> bool"
  where[simp]: "forallInList l f = list_all f l"

definition existsInList :: "'a list \<Rightarrow> ('a \<Rightarrow> bool) \<Rightarrow> bool"
  where[simp]: "existsInList l f = list_ex f l"

definition forallIndex :: "nat \<Rightarrow> (nat \<Rightarrow> bool) \<Rightarrow> bool"
  where[simp]: "forallIndex I f = (list_all id (vec (foreach I (\<lambda> x . (tensor_from_vec [1] [f x])))))"

definition existsIndex :: "nat \<Rightarrow> (nat \<Rightarrow> bool) \<Rightarrow> bool"
  where[simp]: "existsIndex I f = (list_ex id (vec (foreach I (\<lambda> x . (tensor_from_vec [1] [f x])))))"

(* TODO: foreachTuple ? ? ? *)

definition reduceAnd :: "bool tensor \<Rightarrow> bool \<Rightarrow> bool"
  where[simp]: "reduceAnd A i = fold (\<and>) (vec A) i"

definition reduceOr :: "bool tensor \<Rightarrow> bool \<Rightarrow> bool"
  where[simp]: "reduceOr A i = fold (\<or>) (vec A) i"

definition reduceSum :: "('a::plus) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where[simp]: "reduceSum A i = fold (+) (vec A) i"

definition reduceMul :: "('a::times) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where[simp]: "reduceMul A i = fold (*) (vec A) i"

definition reduceMin :: "('a::ord) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where[simp]: "reduceMin A i = fold (min) (vec A) i"

definition reduceMax :: "('a::ord) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where[simp]: "reduceMax A i = fold (max) (vec A) i"

definition eqTensorReduced :: "'a tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[simp]: "eqTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) = (lookup B i)))) True"

definition neTensorReduced :: "'a tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[simp]: "neTensorReduced A B = (\<not>(eqTensorReduced A B))"

definition leqTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[simp]: "leqTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) <= (lookup B i)))) True"

definition ltTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[simp]: "ltTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) < (lookup B i)))) True"

definition geqTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[simp]: "geqTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) >= (lookup B i)))) True"

definition gtTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where[simp]: "gtTensorReduced A B = reduceAnd (tensor_from_lookup (dims A) (\<lambda> i . ((lookup A i) > (lookup B i)))) True"



value "(list_ex id [true, false, false])"

end