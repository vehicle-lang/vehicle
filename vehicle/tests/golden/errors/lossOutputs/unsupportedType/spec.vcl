@network
f : Tensor Real [4] -> Tensor Real [4]

someList : List Nat
someList = 1 :: 2 :: 3 :: nil

@property
prop : Bool
prop = (f [0.0, 0.0, 0.0, 0.0]) ! 0 <= 1.0
