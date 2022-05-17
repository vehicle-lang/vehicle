-- Tests the Fourier-Motzkin elimination algorithm for solving for
-- underconstrained user variables.

network f : Rat -> Rat

forall x y . x >= 0 and y >= 2 and f (x + y) == 0