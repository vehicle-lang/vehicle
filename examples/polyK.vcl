k : forall {s : Type 0}. forall {t : Type 0}. s -> t -> s
k x y = x

kSpecialised : forall {t : Type 0}. Bool -> t -> Bool
kSpecialised x y = k x y

k2 : forall {b : Type 0}. forall {a : Type 0}. a -> b -> a
k2 {b} {a} = k -- TODO: get rid of the need for these implicit lambdas

kRealInt : Real -> Int -> Real
kRealInt = k
