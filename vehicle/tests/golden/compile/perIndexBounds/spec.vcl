-- Test that per-index explicit bounds are correctly extracted
-- by the loss compiler (as opposed to only supporting
-- `forall i . min ! i <= x ! i <= max ! i`).

type InputVector = Tensor Real [3]

a = 0
b = 1
c = 2

@network
f : InputVector -> InputVector

@property
p : Bool
p = forall x .
    (0 <= x ! a <= 1 and
     2 <= x ! b <= 3 and
     4 <= x ! c <= 5) =>
    f x ! a >= 0
