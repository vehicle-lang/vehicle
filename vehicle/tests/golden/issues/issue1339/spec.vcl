@network
f : Tensor Real [2] -> Tensor Real [1, 2]

bounded : Tensor Real [2] -> Bool
bounded x = forall i . 0 <= x ! i <= 1

@property
p : Bool
p = forall x . bounded x => (forall j . f x ! 0 ! j >= 0)
