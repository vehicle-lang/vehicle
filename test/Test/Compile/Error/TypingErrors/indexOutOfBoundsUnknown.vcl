outOfBounds : forallT {n : Nat} . Tensor Nat [n] -> Nat
outOfBounds x = x ! 1