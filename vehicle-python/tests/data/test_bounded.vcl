@network
network : Real -> Real

@property
bounded : Bool
bounded = forall (x : Real) . 0 < x < 1  => 0 < network x  < 1
