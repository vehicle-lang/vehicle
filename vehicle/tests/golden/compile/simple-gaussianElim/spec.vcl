-- Tests the Gaussian elimination algorithm for solving for user variables

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
test1 : Bool
test1 = exists a . 1 >= a >= 0 and f [a + 2] == [0]

{-
-- See https://github.com/vehicle-lang/vehicle/issues/973
@network
g : Tensor Real [2] -> Tensor Real [1]

@property
test2 : Bool
test2 = exists a b . 2 >= a >= 1 and 2 >= b >= 0 and g [a + b , a + 2 * b] == [0]
-}
