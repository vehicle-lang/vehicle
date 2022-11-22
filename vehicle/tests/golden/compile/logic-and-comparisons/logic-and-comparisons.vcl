-- Testing all logical operatives and comparisons

@network
f : Tensor Rat [1] -> Tensor Rat [1]

@property
test1 : Bool
test1 = exists a . a <= 0 or f [a + 2] == [0]

@network
g : Tensor Rat [2] -> Tensor Rat [1]

@property
test2 : Bool
test2 = exists a b . a >= 1 and not (b > a) and g [a + b , a + 2 * b] == [0]

@property
test3 : Bool
test3 = forall a b. not ((a < 0 and b != 0) or g [a, b] != [0])
