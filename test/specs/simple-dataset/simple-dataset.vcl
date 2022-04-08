network f : Rat -> Tensor Rat [4]

dataset d : List (Fin 4)

spec : Prop
spec = forall i inn d . f 0.0 ! i >= 0