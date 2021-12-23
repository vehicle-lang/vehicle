network f : Tensor Real [1] -> Tensor Real [1]

monotonic : Prop
monotonic = every x1 x2 . (x1 ! 0 <= x2 ! 0) => (f x1 ! 0 <= f x2 ! 0)