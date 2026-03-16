@network
f : Tensor Real [1] -> Tensor Real [1]

boundedByOne : Tensor Real [1] -> Bool
boundedByOne x = forall i . -1 <= x ! i <= 1

@property
boundedRobust : Bool
boundedRobust = forall x x_hat .
    boundedByOne x and boundedByOne x_hat =>
    boundedByOne ((f x) - (f x_hat))
