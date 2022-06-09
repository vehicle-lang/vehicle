network f : Tensor Rat [1] -> Tensor Rat [1]

property : Bool
property = forall y . not (not (exists x . f [x] != [y]))