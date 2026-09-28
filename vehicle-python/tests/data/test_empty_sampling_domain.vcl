@network
network : Tensor Real [1] -> Tensor Real [1]

@parameter
lower : Real

@parameter
upper : Real

@property
p : Bool
p = forall (x : Real) . lower <= x <= upper => network [x] ! 0 <= 0
