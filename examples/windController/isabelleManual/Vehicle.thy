theory Vehicle
  imports
    Complex_Main
    "Deep_Learning.Tensor"
begin

definition foreach :: "nat list \<Rightarrow> (nat list \<Rightarrow> 'a) \<Rightarrow> 'a tensor"
  where "foreach = tensor_from_lookup"

definition forallInList :: "'a list \<Rightarrow> ('a \<Rightarrow> bool) \<Rightarrow> bool"
  where "forallInList l f = list_all f l"

definition existsInList :: "'a list \<Rightarrow> ('a \<Rightarrow> bool) \<Rightarrow> bool"
  where "existsInList l f = list_ex f l"

definition forallIndex :: "nat list \<Rightarrow> (nat list \<Rightarrow> bool) \<Rightarrow> bool"
  where "forallIndex I f = (list_all id (vec (foreach I f)))"

definition existsIndex :: "nat list \<Rightarrow> (nat list \<Rightarrow> bool) \<Rightarrow> bool"
  where "existsIndex I f = (list_ex id (vec (foreach I f)))"

(* TODO: foreachTuple ? ? ? *)

definition reduceAnd :: "bool tensor \<Rightarrow> bool \<Rightarrow> bool"
  where "reduceAnd A i = fold (\<and>) (vec A) i"

definition reduceOr :: "bool tensor \<Rightarrow> bool \<Rightarrow> bool"
  where "reduceOr A i = fold (\<or>) (vec A) i"

definition reduceSum :: "('a::plus) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where "reduceSum A i = fold (+) (vec A) i"

definition reduceMul :: "('a::times) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where "reduceMul A i = fold (*) (vec A) i"

definition reduceMin :: "('a::ord) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where "reduceMin A i = fold (min) (vec A) i"

definition reduceMax :: "('a::ord) tensor \<Rightarrow> 'a \<Rightarrow> 'a"
  where "reduceMax A i = fold (max) (vec A) i"

definition eqTensorReduced :: "'a tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where "eqTensorReduced A B = reduceAnd (foreach (dims A) (\<lambda> i . ((lookup A i) = (lookup B i)))) True"

definition neTensorReduced :: "'a tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where "neTensorReduced A B = (\<not>(eqTensorReduced A B))"

definition leqTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where "leqTensorReduced A B = reduceAnd (foreach (dims A) (\<lambda> i . ((lookup A i) <= (lookup B i)))) True"

definition ltTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where "ltTensorReduced A B = reduceAnd (foreach (dims A) (\<lambda> i . ((lookup A i) < (lookup B i)))) True"

definition geqTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where "geqTensorReduced A B = reduceAnd (foreach (dims A) (\<lambda> i . ((lookup A i) >= (lookup B i)))) True"

definition gtTensorReduced :: "('a::ord) tensor \<Rightarrow> 'a tensor \<Rightarrow> bool"
  where "gtTensorReduced A B = reduceAnd (foreach (dims A) (\<lambda> i . ((lookup A i) > (lookup B i)))) True"



value "(list_ex id [true, false, false])"

end