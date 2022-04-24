implicit parameter n : Nat

dataset d : Tensor Rat [n]

network f : Rat -> Rat

positive : Bool
positive = forall x in d . f x > 0