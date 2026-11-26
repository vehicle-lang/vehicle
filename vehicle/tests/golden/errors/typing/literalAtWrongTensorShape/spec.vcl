-- Tests the error when a scalar literal is used where a tensor is expected (see #976).

@network
f : Tensor Real [2] -> Tensor Real [1]

@property
p : Bool
p = f 0 ! 0 > 0
