-- Tests the Fourier-Motzkin elimination algorithm for solving for
-- underconstrained user variables.

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
p1 : Bool
p1 = exists x . f [ x ] ! 0 >= 0

@property
p2 : Bool
p2 = p1
