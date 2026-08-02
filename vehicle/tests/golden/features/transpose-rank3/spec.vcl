@network
g : Tensor Real [2] -> Tensor Real [2, 3, 4]

out3d : Tensor Real [2, 3, 4]
out3d = g [0.0, 0.0]

@property
allTransposedBounded : Bool
allTransposedBounded = forall i j k . (transpose out3d) ! i ! j ! k <= 1.0

@property
firstAxisSlice : Bool
firstAxisSlice = forall j k . 0.0 <= (transpose out3d) ! 0 ! j ! k
