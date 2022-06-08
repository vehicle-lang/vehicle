-- Tests the Gaussian elimination algorithm for solving for user variables

network f : Tensor Rat [1] -> Tensor Rat [1]

test1 : Bool
test1 = exists a . a >= 0 and f [a + 2] == [0]

network g : Tensor Rat [2] -> Tensor Rat [1]

test2 : Bool
test2 = exists a b . a >= 1 and b >= a and g [a + b , a + 2 * b] == [0]