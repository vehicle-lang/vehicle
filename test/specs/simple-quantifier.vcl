
bool : Prop
bool = forall (x : Int) . True

expandedExpr : Prop
expandedExpr = forall (x : Tensor Int [2]) . x ! 0 == x ! 1