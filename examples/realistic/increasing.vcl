
network f : Tensor Real [1] -> Tensor Real [1]

monotonic : Prop
monotonic = every x . (let y = f x in (x ! 0 <= y ! 0))