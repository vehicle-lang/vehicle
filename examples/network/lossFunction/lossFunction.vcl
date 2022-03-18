network f : Real -> Real

bounded : Prop
bounded = forall x . 0 < x < 1  => f x < 1