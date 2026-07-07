@network
f : Tensor Real [1] -> Tensor Real [1]

wrapper : Tensor Real [1] -> Tensor Real [1]
wrapper x = f x

result : Tensor Real [1]
result = wrapper [0.0]

@property
p : Bool
p = result ! 0 >= 0.0
