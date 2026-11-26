-- Tests that an error is reported for every network application whose input
-- cannot be bounded, and that the fix refers to the user's quantified variables.

@network
f : Tensor Real [2] -> Tensor Real [1]

@network
g : Tensor Real [2] -> Tensor Real [1]

@property
p : Bool
p = forall x y . 0 <= x ! 0 <= 1 => f x ! 0 <= g (y + y) ! 0
