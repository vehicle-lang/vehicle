@property
prop1 : Bool
prop1 = exists (x : Real) . 0 <= x <= 1

@property
prop2 : Bool
prop2 = exists (x : Real) . (0 <= x <= 1) or (0.5 <= x <= 1.5)
