-- Const-free rank-2 transpose: keeps the `transpose` builtin alive into the
-- ITP backends (the tensor is a network output, so it cannot be folded away)
-- while avoiding the unrelated `ConstTensor`-ITP bug that `spec.vcl`'s
-- `positionUnderLimit` trips (see const-tensor-itp-bug.md at the repo root).
-- Used by the Rocq/Isabelle *Verify* golden steps to check the generated ITP
-- output actually elaborates.

@network
f : Tensor Real [2] -> Tensor Real [3, 2]

mat : Tensor Real [3, 2]
mat = f [0.0, 0.0]

@property
transposedNonNegative : Bool
transposedNonNegative = forall i j . (transpose mat) ! i ! j >= 0.0
