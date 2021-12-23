network f : Real -> Real

monotonic : Prop
monotonic = every x1 x2 . (x1 <= x2) => (f x1 <= f x2)