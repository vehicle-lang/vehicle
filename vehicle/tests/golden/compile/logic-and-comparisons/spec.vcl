-- Testing all logical operatives and comparisons

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
test1 : Bool
test1 = exists a . a <= 0 or f [a + 2] == [0]

@network
g : Tensor Real [2] -> Tensor Real [1]

@property
test2 : Bool
test2 = exists a b . a >= 1 and not (b > a) and g [a + b , a + 2 * b] == [0]

@property
test3 : Bool
test3 = forall a b. not ((a < 0 and b != 0) or g [a, b] != [0])
