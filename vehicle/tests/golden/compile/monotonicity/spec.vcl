@network
f : Real -> Real

@property
monotonic : Bool
monotonic = forall x1 x2 . (0 < x1 < 1 and 0 < x2 < 1 and x1 <= x2) => (f x1 <= f x2)
