network f : Tensor Rat [1] -> Tensor Rat [1]

property : Bool
property = forall y . not (forall x . f [x] != [y])