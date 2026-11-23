xs : Vector (Tensor Real [1]) 1
xs = [foreach i . 0]

@property
p : Bool
p = 0 <= xs ! 0 ! 0
