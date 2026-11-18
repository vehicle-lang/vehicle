@network
f : Tensor Real [2] -> Real

-- `x ! 0 >= x ! 1` should not be converted to a bound
@property
p : Bool
p = forall x . (x ! 0 >= x ! 1) => f x >= 0.5

-- `(forall i . x ! 0 <= x ! i)` should not be converted to a bound
@property
q : Bool
q = forall x . (forall i . x ! 0 <= x ! i) => f x >= 0.5

-- `x ! i <= y ! 0` should not be converted to a bound
@property
r : Bool
r = forall x y . (forall i . x ! i <= y ! 0) => f x >= f y
