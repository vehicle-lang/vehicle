
network f : Tensor Real [1] -> Tensor Real [1]

monotonic : Prop
monotonic = every x1 x2 . (let y1 = f x1 in (let y2 = f x2 in
  ((x1 ! 0 <= x2 ! 0) => (y1 ! 0 <= y2 ! 0))))