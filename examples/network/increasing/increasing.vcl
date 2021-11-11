{-
network f : Tensor Real [1] -> Tensor Real [1]

increasing : Prop
increasing = every x . (let y = f x in (x ! 0 <= y ! 0))
-}

network f : Tensor Real [1] -> Tensor Real [1]

increasing : Prop
increasing = every x . (let y = f x in (x ! 0 <= y ! 0))