-- Tests the Gaussian elimination algorithm for solving for user variables

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
test1 : Bool
test1 = exists a . a >= 0 and f [a + 2] == [0]

@network
g : Tensor Real [2] -> Tensor Real [1]

@property
test2 : Bool
test2 = exists a b . a >= 1 and b >= a and g [a + b , a + 2 * b] == [0]
