@network
f : Tensor Real [2] -> Tensor Real [3, 2]

mat : Tensor Real [3, 2]
mat = f [0.0, 0.0]

@property
transposedNonNegative : Bool
transposedNonNegative = forall i j . (transpose mat) ! i ! j >= 0.0
