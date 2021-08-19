k : forall (s : Type 0). forall (t : Type 0). s -> t -> s
k s t x y = x

{-
k1 : forall t. Real -> t -> Real
k1 = k {Real}

k2 : forall b. forall a. a -> b -> a
k2 {b} {a} = k {a} {b}

kRealInt : Real -> Int -> Real
kRealInt = k {Real} {Int}
-}
