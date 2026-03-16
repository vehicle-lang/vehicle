@network
f : Real -> Real

@property
p : Bool
p = forall x. 0 <= x <= 1 => (if f x > 0.5 then 1 else 0) != 1
