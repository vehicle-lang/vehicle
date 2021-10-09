
bool : Prop
bool = every (x : Int) . True

expandedExpr : Prop
expandedExpr = every (x : (Tensor Int [2])) . x ! 0 == x ! 1

-- every x0 : Int . every x1 : Int . x0 == x1

-- [x0, x1]