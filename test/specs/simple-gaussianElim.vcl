-- Tests the Gaussian elimination algorithm for solving for user variables

network f : Rat -> Rat

test1 : Bool
test1 = exists a . a >= 0 and f (a + 2) == 0

network g : Rat -> Rat -> Rat

test2 : Bool
test2 = exists a b . a >= 1 and b >= a and g (a + b) (a + 2 * b) == 0