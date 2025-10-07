theory WindControllerSpec
  imports
    Complex_Main
    "Deep_Learning.Tensor"
    "Vehicle"
begin
type_synonym R = "real"

typedef InputVector = "{ a :: R tensor. (dims a) = [2] }"
  using dims_tensor_from_lookup by blast

definition currentSensor :: "nat list"
  where "currentSensor = [0]"

definition previousSensor :: "nat list"
  where "previousSensor = [1]"

typedef OutputVector = "{ a :: R tensor. (dims a) = [1] }"
  using dims_tensor_from_lookup by blast

definition velocity :: "nat list"
  where "velocity = [0]"

definition normalise :: "InputVector \<Rightarrow> InputVector"
  where "normalise x = Abs_InputVector (foreach [2] ((\<lambda> i . (((lookup (Rep_InputVector x) i) +4)/8))))"

definition safeInput :: "InputVector \<Rightarrow> bool"
  where "safeInput x = forallIndex [2] (\<lambda> i . (-13/4 \<le> lookup (Rep_InputVector x) i) \<and> (-13/4 \<le> lookup (Rep_InputVector x) i))"

definition safeOutput :: "(InputVector \<Rightarrow> OutputVector) \<Rightarrow> InputVector \<Rightarrow> bool"
  where "safeOutput controller x =
          (let
              y = (lookup (Rep_OutputVector (controller x)) velocity)
            in
              (-1.25 < (y + 2 * (lookup (Rep_InputVector x) currentSensor) - (lookup (Rep_InputVector x) previousSensor))) \<and>
               ((y + 2 * (lookup (Rep_InputVector x) currentSensor) - (lookup (Rep_InputVector x) previousSensor)) < 1.25) )"

definition safe :: "(InputVector \<Rightarrow> OutputVector) \<Rightarrow> bool"
  where "safe controller = (\<forall> x . ((safeInput x) \<longrightarrow> (safeOutput controller x)))"

locale WindControllerSpec =
  fixes controller :: "InputVector \<Rightarrow> OutputVector"
  assumes safe: "safe controller"
begin

end

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


end