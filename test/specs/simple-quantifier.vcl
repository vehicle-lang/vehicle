
bool : Bool
bool = forall (x : Int) . True

expandedExpr : Bool
expandedExpr = forall (x : Tensor Int [2]) . x ! 0 == x ! 1

multiple : Bool
multiple = forall (x : Nat) . forall (y : Nat) . x == y