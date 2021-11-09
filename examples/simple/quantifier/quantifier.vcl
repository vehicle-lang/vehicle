
bool : Prop
bool = every (x : Int) . True

expandedExpr : Prop
expandedExpr = every (x : Tensor Int [2]) . x ! 0 == x ! 1