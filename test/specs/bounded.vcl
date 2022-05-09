network f : Real -> Real

bounded : Bool
bounded = forall x . 0 < x < 1  => 0 < f x < 1