-- Tests the Fourier-Motzkin elimination algorithm for solving for
-- underconstrained user variables.

network f : Tensor Rat [1] -> Tensor Rat [1]

unusedVar : Bool
unusedVar = exists x (y : Rat) . f [ x ] ! 0 >= 0

underConstrainedVar1 : Bool
underConstrainedVar1 = exists x y . x >= 1 and y >= 2 and f [x + y] ! 0 >= 0

underConstrainedVar2 : Bool
underConstrainedVar2 = exists x y . x >= 1 and 2 * y >= 2 and f [ 2 * x + y ] ! 0 >= 0

network g : Tensor Rat [2] -> Tensor Rat [1]

underConstrainedVars : Bool
underConstrainedVars = exists (x : Tensor Rat [4]) .
  x ! 3 >= 2 and
  x ! 2 + x ! 3 >= 1 and
  x ! 1 + x ! 0 >= 2.5 and
  f [ x ! 0 + x ! 1 ] ! 0 >= 2