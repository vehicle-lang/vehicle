@network
f : Tensor Real [1] -> Tensor Real [1]

boundedByOne : Tensor Real [1] -> Bool
boundedByOne x = forall i . -1 <= x ! i <= 1

@property
boundedRobust : Bool
boundedRobust = forall x_hat . boundedByOne (f x_hat)
