-- Tests the Fourier-Motzkin elimination algorithm for solving for
-- underconstrained user variables.

network f : Rat -> Rat

forall x y . f (x + y) == 0