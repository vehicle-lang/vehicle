-- Tests the Fourier-Motzkin elimination algorithm for solving for
-- underconstrained user variables.

@network
f : Tensor Real [1] -> Tensor Real [1]

@property
unusedVar : Bool
unusedVar = exists x (y : Real) . 0 < x < 1 and f [ x ] ! 0 >= 0

@property
underConstrainedVar1 : Bool
underConstrainedVar1 = exists x y . x < 2 and y < 3 and x >= 1 and y >= 2 and f [x + y] ! 0 >= 0

@property
underConstrainedVar2 : Bool
underConstrainedVar2 = exists x y . x < 2 and y < 3 and x >= 1 and 2 * y >= 2 and f [ 2 * x + y ] ! 0 >= 0

@property
underConstrainedVars : Bool
underConstrainedVars = exists (x : Tensor Real [5]) .
  (forall i . 0 < x ! i < 10) and
  x ! 3 >= 2 and
  x ! 2 + x ! 3 >= 1 and
  x ! 1 - 2 * (x ! 3) >= 2.5 and
  f [ x ! 0 + x ! 1 ] ! 0 >= 2
