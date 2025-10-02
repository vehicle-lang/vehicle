@network
f : Real -> Real

@property
increasing : Bool
increasing = forall x . 0 < x < 1 => x <= f x
